;; Copyright(c) 2012 Aki Helin
;; Copyright(c) 2014 - 2023 Yuriy Chumak
;;
;; Otus Lisp is available under 2 licenses:
;; 'MIT License' or 'GNU LGPLv3 License'.
;;

(print "Loading code...")
(define build-start (time-ms))

; drop all libraries except (otus core) which is
; a virtual olvm language primitives library.
(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(otus core)))
      *libraries*))

; reimport core language:

; предварительная загрузка зависимостей scheme core,
; (иначе импорт сбойнет)
(import (src vm))       ;; olvm high level interface
(import (srfi 16))      ;; case-lambda
(import (srfi 87))      ;; "=>" clauses in case
(import (srfi 71))      ;; extended LET-syntax for multiple values
(import (scheme core))  ;; core Scheme functions and primitives

;; todo: вообще-то тут надо бы интернер очистить ??

;; forget everything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *version* *path* stdin stdout stderr build-start)

(import (otus core))    ;; olvm promops and ol special forms
(import (scheme core))  ;; базовый языковый ...
(import (scheme base))  ;; ... набор Scheme
(import (otus lisp))    ;; а теперь загрузим ВСЕ, чтобы успешно отработал bytecode-interner (и сократил размер образа)

;; core implementation features, used by cond-expand
(define *features* '(
   otus-lisp
   r7rs ; R7RS-small standard
   srfi-16 ; case-lambda
   srfi-87 ; <= in cases
   srfi-71 ; extended LET-syntax for multiple values
)) ; scheme-compliant naming

(define *loaded* '())   ;; can be removed soon, used by old ,load and ,require

; let's prepare a new Ol compiler
(import (lang intern))
(import (lang threading))

(import (lang gensym))
(import (lang env))
(import (lang macro))
(import (lang sexp))
(import (lang error))

(import (lang ast))
(import (lang fixedpoint))
(import (lang alpha))
(import (lang cps))
(import (lang closure))
(import (lang assemble))
(import (lang rtl))

(import (lang eval))
(import (lang embed))

; replace old (otus core) to the new one, (only (lang eval) *otus-core*)
(define *libraries* ; заменим старую (otus core) на новую из (lang eval)
   (cons
      (cons '(otus core) *otus-core*)
      (keep
         (λ (x)
            (not (equal? (car x) '(otus core))))
         *libraries*)))

;
; features
(define *features* (append *features* `(
   ; math:
   exact-closed   ; The algebraic operations +, -, *, and expt where the
                  ; second argument is a non-negative integer produce exact
                  ; values given exact inputs.
   exact-complex  ; Exact complex numbers are provided.
   ratios         ; / with exact arguments produces an exact result when
                  ; the divisor is nonzero.

   full-unicode   ; All Unicode characters present in Unicode version 6.0
                  ; are supported as Scheme characters (actually, 14.0.0).
   immutable)))   ; todo: ?

(define *features* (append *features* '(srfi-0))) ; cond-expand

;; -------------

(import (only (owl io)
   system-stderr))

(print "Code loaded at " (- (time-ms) build-start) " ms.")

;; a temporary O(n) way to get some space in the heap

;; fixme: allow a faster way to allocate memory
;; n-megs → _
(define (ensure-free-heap-space megs)
   (when (> megs 0)
      (define word-size (size (vm:cast 0 type-vptr)))
      (define blocksize (<< 1 (* (- word-size 1) 8)))
      (let loop ((bytes (* megs 1048576)) (out #null))
         (if (<= bytes 0)
            out
         else
            (define block (min bytes blocksize))
            (loop (- bytes block) (cons (make-bytevector block) out))))))

(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it

;; enter sandbox with at least n-megs of free space in heap, or exit
(define (sandbox n-megs)
   ; allocate n-megs heap space, which is necessary given that
   ;  we won't be able to get more memory after entering seccomp.
   (define garbage (ensure-free-heap-space
      (if (string? n-megs) (string->number n-megs) 1)))
   (syscall 157))


;; todo: share the modules instead later
(define-syntax share-bindings
   (syntax-rules (defined)
      ((share-bindings) null)
      ((share-bindings this . rest)
         (cons
            (cons 'this
               ['defined (mkval this)])
            (share-bindings . rest)))))

(define shared-bindings (share-bindings
   *features*
   *libraries*))      ;; all currently loaded libraries

(define initial-environment-sans-macros
   (fold
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *otus-core*
      shared-bindings))

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((otus lisp))
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; errors
(import (src vm))
(import (lang primop))


;; help strings
(define copyright (symbol->string '
|Copyright (c) 2014-2023 Yuriy Chumak
   License LGPLv3+: GNU LGPL version 3 or later <http://gnu.org/licenses/>
   License MIT: <https://en.wikipedia.org/wiki/MIT_License>
   This is free software: you are free to change and redistribute it.
   There is NO WARRANTY, to the extent permitted by law.|))

(define help (symbol->string '
|Usage: ol [OPTION]... [input-file] [file-options]

   --home=<path(es)>   run in virtual environment, divide pathes by ':'
   --sandbox           run in sandboxed environment (if supported)
   --sandbox=<number>  run sandboxed with <number> Megs heap allocated

   --interactive       make execution envorinment interactive
   --no-interactive    make execution envorinment non interactive

   --help              this help
   -v, --version       print short or long version info
   --version=<string>  override internal version string
   --                  end of options

Otus Lisp homepage: <https://github.com/otus-lisp/>.|))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; new repl image
;;;

; entry point of the compiled image
; (called after starting mcp, symbol and bytecode interners)
(define (main vm-args)

   (define (starts-with? string prefix)
      (and (<= (string-length prefix) (string-length string))
            (string-eq? prefix (substring string 0 (string-length prefix)))))

   ;; (print "vm-args: " vm-args)

   (let*((options vm-args
            (let loop ((options #empty) (args vm-args))
               (cond
                  ((null? args)
                     (values options #null))

                  ((string-eq? (car args) "--help")
                     (print help)
                     (halt 0))
                  ;; version manipulation
                  ((string-eq? (car args) "-v")
                     (print "ol (Otus Lisp) " (get options 'version (cdr *version*)))
                     (halt 0))
                  ((string-eq? (car args) "--version")
                     (print "ol (Otus Lisp) " (get options 'version (cdr *version*)))
                     (print copyright)
                     (halt 0))


                  ((starts-with? (car args) "--version=")
                     (loop (put options 'version
                              (substring (car args) 10))
                           (cdr args)))

                  ;; additional options
                  ((string-eq? (car args) "--sandbox")
                     (loop (put options 'sandbox #t) (cdr args)))
                  ((starts-with? (car args) "--sandbox=")
                     (loop (put options 'sandbox
                              (substring (car args) 10))
                           (cdr args)))

                  ((string-eq? (car args) "--interactive")
                     (loop (put options 'interactive #t) (cdr args)))
                  ((string-eq? (car args) "--no-interactive")
                     (loop (put options 'interactive #f) (cdr args)))

                  ;; special case - use embed REPL version
                  ((string-eq? (car args) "--embed")
                     (loop (put options 'embed #t) (cdr args)))

                  ;; new option: compile into binary
                  ((starts-with? (car args) "--compile-to=")
                     (loop (put options 'compile
                              (substring (car args) 13))
                           (cdr args)))

                  ;; home
                  ((string-eq? (car args) "--home")
                     (print "use --home=<path>")
                     (halt 1))
                  ((starts-with? (car args) "--home=")
                     (loop (put options 'home
                              (substring (car args) 7 (string-length (car args))))
                           (cdr args)))
                     
                  ;; end of options and unknown option
                  ((string-eq? (car args) "--")
                     (let*((file args (uncons (cdr args) #null)))
                        (values
                           (put options 'file file)
                           args)))
                  ((starts-with? (car args) "--")
                     (print "unknown command line option '" (car args) "'")
                     (halt 0))

                  (else
                     (values
                        (put options 'file (car args))
                        (cdr args))))))

         (file (getf options 'file))
         (file (when (string? file)
                  (unless (string-eq? file "-")
                     (let ((port (open-input-file file)))
                        (unless port
                           (print "error: can't open file '" file "'")
                           (halt 3))
                        port))))
         (file (or file stdin))


         (sandbox? (getf options 'sandbox))
         (interactive? (get options 'interactive (syscall 16 file 19))) ; isatty
         (embed? (getf options 'embed))
         (compile? (getf options 'compile))

         (home (or (getf options 'home) ; via command line
                   (syscall 1016 "OL_HOME")   ; guessed by olvm if not exists
                   #null)) ; standalone?
         (command-line vm-args)

         (version (cons "OL" (get options 'version (cdr *version*))))
         (env (fold
                  (λ (env defn)
                     (env-set env (car defn) (cdr defn)))
                  initial-environment
                  (list
                     ;(cons '*owl-names* initial-names)                                       ; windows workaround below:
                     (cons '*path* (cons "." ((if (string=? (ref (or (syscall 63) [""]) 1) "Windows") c/;/ c/:/) home)))
                     (cons '*interactive* interactive?)
                     (cons '*command-line* command-line)
                     ; (cons 'command-line (lambda () command-line)) ;; use (scheme process-context) library instead
                     (cons '*vm-args* vm-args) ; deprecated
                     (cons '*version* version)
                     ; 
                     (cons '*features* (let*((*features* (cons* '|ol-2.3| *features*))
                                             ; endiannes
                                             (*features* (let ((one (vm:cast 1 type-vptr)))
                                                            (cond
                                                               ((eq? (ref one 0) 1)
                                                                  (append *features* '(little-endian)))
                                                               ((eq? (ref one (- (size one) 1)) 1)
                                                                  (append *features* '(big-endian)))
                                                               ((eq? (ref one 1) 1)
                                                                  (append *features* '(middle-endian)))
                                                               (else
                                                                  *features*))))
                                             ; posix, string-port, ieee-float
                                             (*features* (let ((features (vm:features)))
                                                            (append *features* (reverse
                                                               (fold (lambda (l symbol mask)
                                                                        (if (eq? (band features mask) 0)
                                                                           l
                                                                           (cons symbol l)))
                                                                  #null
                                                                  '(posix      string-port  ieee-float)
                                                                  '(#o1000000  #o0400       #o0020    ))))))

                                             ; OS/Platform
                                             (*features* (let ((uname (syscall 63)))
                                                            (if uname
                                                               (append *features* (list
                                                                     (string->symbol (ref uname 1))  ; OS
                                                                     (string->symbol (ref uname 5)))) ; Platform
                                                               *features*))))
                                          *features*))
                     (cons 'describe-ol-error verbose-ol-error)
                     ;(cons '*scheme* 'r7rs)
                     (cons '*sandbox* sandbox?)))))
         ; go:
         (if sandbox?
            (unless (sandbox sandbox?)
               (system-stderr "Failed to enter the sandbox.\nYou must have SECCOMP support enabled.\n")
               (halt exit-seccomp-failed)))


         ; ohai:
         (if interactive?
            (print "Welcome to Otus Lisp " (cdr version)
               (if sandbox? ", you feel restricted" "")
               "\n"
               (if embed? "" "type ',help' to help, ',quit' to end session.")))

         (if embed?
            (let*((this (cons (vm:pin env) 0))
                  (eval (lambda (exp args)
                           (case exp
                              (['ok value env]
                                 (vm:unpin (car this))
                                 (set-car! this (vm:pin env))
                                 (if (null? args)
                                    value
                                    (apply value args)))
                              (else is error
                                 (print-to stderr "error: " (ref error 2))
                                 #false))))
                  (evaluate (lambda (expression)
                           (halt
                              (let*((env (vm:deref (car this)))
                                    (exp args (uncons expression #f)))
                                 (case (type exp)
                                    (type-string
                                       (eval (eval-string env exp) args))
                                    (type-string-wide
                                       (eval (eval-string env exp) args))
                                    (type-enum+
                                       (eval (eval-repl (vm:deref exp) env #f evaluate) args))
                                    (type-bytevector
                                       (eval (eval-repl (fasl-decode (bytevector->list exp) #f) (vm:deref (car this)) #f evaluate) args))))))))
               (halt (vm:pin evaluate)))
         else
            ; regular repl:
            (actor ['repl] (lambda ()
               (let*((lastone (repl-loop env file))
                     (lastone (if compile?
                                 (let*((path compile?)
                                       (port (open-output-file path)))
                                    (if (not port)
                                    then
                                       (print-to stderr "Could not open " path " for write")
                                       #false ; error
                                    else
                                       (write-bytes port (fasl-encode (make-entry lastone)))
                                       (close-port port)))
                                 lastone)))
               (exit-thread lastone)))))))

;;;
;;; Dump the new repl
;;;

(print "Compiling ...")

(import (otus fasl))
(let*((path "boot.fasl")
      (port ;; where to save the result
         (open-output-file path))

      (bytes ;; encode entry as "autorun" function
         (fasl-encode (make-entry main))))
   (if (not port)
   then
      (print "Could not open " path " for write")
      (exit -1) ; error
   else ;; just save the fasl dump
      (write-bytes port bytes)
      (close-port port)
      (print "Output written at " (- (time-ms) build-start) " ms.")
      (exit 0))) ; ok
