;;
;; an Otus Lisp read-eval-print loop (REPL) binary image compiler
;;

#| Copyright(c) 2012 Aki Helin
 | Copyright(c) 2014 - 2017 Yuriy Chumak
 |
 | This program is free software;  you can redistribute it and/or
 | modify it under the terms of the GNU General Public License as
 | published by the Free Software Foundation; either version 2 of
 | the License, or (at your option) any later version.
 | 
 | This program is distributed in the hope that it will be useful,
 | but WITHOUT ANY WARRANTY; without even the implied warranty of
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 | 
 | You should have received a copy of the GNU GPL along with this
 | program.           If not, see <http://www.gnu.org/licenses/>.
 |#

(define build-start (time-ms))

; forget all other libraries to have them be reloaded and rebuilt
; (src olvm) содержит список базовых элементов языка
(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(src olvm)))
      *libraries*))

(import (src vm))   ;; команды виртуальной машины
(import (r5rs core)) ;; базовый языковый набор ol


;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *codes* *vm-args* wait stdin stdout stderr set-ticker-value build-start)


;;;
;;; Time for a new REPL
;;;
(import (src olvm))     ;; get special forms, primops and define-syntax (virtual library)

;; this should later be just a sequence of imports followed by a fasl dump
(import (r5rs core))    ;; get define, define-library, import, ... from the just loaded

(define *include-dirs* '(".")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)
(define *loaded* '())   ;; can be removed soon, was used by old ,load and ,require


;; shared parameters, librarize later or remove if possible

;; http://semver.org/lang/ru/
(define *owl-version* "1.1")
(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it
(define max-object-size #xffff)  ; todo: change as dependent of word size


(import (otus lisp))

(import (lang intern))
(import (owl parse))

(import (lang gensym))
(import (lang env))
(import (lang macro))
(import (lang sexp))

(import (lang ast))
(import (lang fixedpoint))
(import (lang cps))
(import (lang alpha))

(import (lang thread))
(import (lang assemble))
(import (lang closure))
(import (lang compile))


(define error-tag "err")

(define (error? x)
   (and (tuple? x)
      (eq? (ref x 1) error-tag)))

(import (owl time))
(import (owl fasl))
(import (scheme misc))

;; fixme: should sleep one round to get a timing, and then use avg of the last one(s) to make an educated guess
(define (sleep ms)
   (lets ((end (+ ms (time-ms))))
      (let loop ()
         ;(print (interop 18 1 1))
         (let ((now (time-ms)))
            (if (> now end)
               now
               (begin (interact sleeper-id 50) (loop)))))))

; -> mcp gets <cont> 5 reason info
; (run <mcp-cont> thunk quantum) -> result

(define input-chunk-size  1024)
(define output-chunk-size 4096)

(define-syntax share-bindings
   (syntax-rules (defined)
      ((share-bindings) null)
      ((share-bindings this . rest)
         (cons
            (cons 'this
               (tuple 'defined (mkval this)))
            (share-bindings . rest)))))

(define (share-modules mods)
   (for null mods
      (λ (envl mod)
         (append (ff->list mod) envl))))

;(import (owl random))

(import (owl args))
(import (owl sys))

;; implementation features, used by cond-expand
(define *features*
   (cons
      (string->symbol (string-append "owl-lisp-" *owl-version*))
      '(owl-lisp r7rs exact-closed ratios exact-complex full-unicode immutable)))
      ;;          ^
      ;;          '-- to be a fairly large subset of at least, so adding this

(import (lang eval))

;; push it to libraries for sharing, replacing the old one
(define *libraries* ; заменим старую (src olvm) на новую
   (cons
      (cons '(src olvm) *src-olvm*)
      (keep (λ (x) (not (equal? (car x) '(src olvm)))) *libraries*)))

;; todo: share the modules instead later
(define shared-misc
   (share-bindings
      *features*
      *include-dirs*
      *libraries*))      ;; all currently loaded libraries



;;;
;;; MCP, master control program and the thread controller
;;;

(define shared-bindings shared-misc)

(define initial-environment-sans-macros
   (fold
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *src-olvm*
      shared-bindings))

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((otus lisp))
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))

(define *version*
   (let loop ((args *vm-args*))
      (if (null? args)
         (cdr (vm:version))
      (if (string-eq? (car args) "--version")
         (if (null? (cdr args))
            (runtime-error "no version in command line" args)
            (cadr args))
         (loop (cdr args))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; new repl image
;;;

;; say hi if interactive mode and fail if cannot do so (the rest are done using
;; repl-prompt. this should too, actually)
(define (get-main-entry symbols codes)
   (let*((initial-names   *owl-names*)
         (initial-version *owl-version*))
      ; main: / entry point of the compiled image
      (λ (vm-args)
         ;(print "//vm-args: " vm-args)
         ;; now we're running in the new repl
         (start-thread-controller
            (list ;1 thread
               (tuple 'init
                  (λ ()
                     (let ((IN  (cast (string->integer (car  vm-args)) type-port))
                           (OUT (cast (string->integer (cadr vm-args)) type-port)))
                     (fork-server 'repl (lambda ()
                        ;; get basic io running
                        (io:init)

                        ;; repl needs symbol and bytecode interning,
                        ;;  which is handled by this threads
                        (fork-intern-interner symbols)
                        (fork-bytecode-interner codes)

                        ;; set a signal handler which stop evaluation instead of owl
                        ;; if a repl eval thread is running
                        (set-signal-action repl-signal-handler) ; TODO: change

                        ;; repl
                        (exit-owl
                           (let*((home (or (getenv "OL_HOME")
                                           (cond
                                              ((string-eq? (ref (uname) 1) "Windows") "C:/Program Files/OL")
                                              (else "/usr/lib/ol")))) ; Linux, *BSD, etc.
                                 (version (cons "OL" *version*))
                                 (env (fold
                                          (λ (env defn)
                                             (env-set env (car defn) (cdr defn)))
                                          initial-environment
                                          (list
                                             (cons '*owl-names*   initial-names)
                                             (cons '*owl-version* initial-version)
                                             (cons '*include-dirs* (list "." home))
                                             (cons '*interactive* #false)
                                             (cons '*vm-args* vm-args)
                                             (cons '*version* version)
                                             (cons '*sandbox* #false)

                                             (cons 'IN IN)
                                             (cons 'OUT OUT)
                                          ))))
                              (repl-trampoline env IN)))))))))
            )))) ; no threads state


;(define symbols (symbols-of get-main-entry))
;(define codes   (codes-of   get-main-entry))

;;;
;;; Dump the new repl
;;;

;--
(define (symbols-of node)
   (define tag (list 'syms))

   (define (walk trail node)
      (cond
         ((value? node) trail)
         ((get trail node #false) trail)
         ((symbol? node)
            (let ((trail (put trail node 1)))
               (put trail tag
                  (cons node (get trail tag null)))))
         ((raw? node)
            (cond
               ((eq? (type node) type-bytecode) #t)
               ((eq? (type node) type-string) #t)
               ((eq? (type node) type-port) #t)
               ((eq? (type node) type-vector-raw) #t)
               (else (print "unknown raw object: " node)))
            trail)
         (else
            (fold walk
               (put trail node #true)
               (tuple->list node)))))
   (define trail
      (walk (put empty tag null) node))

   (get
      (walk (put empty tag null) node)
      tag null))

;--
(define (code-refs seen obj)
   (cond
      ((value? obj) (values seen empty))
      ((bytecode? obj)
         (values seen (put empty obj 1)))
      ((get seen obj #false) =>
         (λ (here) (values seen here)))
      (else
         (let loop ((seen seen) (lst (tuple->list obj)) (here empty))
            (if (null? lst)
               (values (put seen obj here) here)
               (lets ((seen this (code-refs seen (car lst))))
                  (loop seen (cdr lst)
                     (ff-union this here +))))))))
(define (codes-of ob)
   (lets ((refs this (code-refs empty ob)))
      (ff-fold (λ (out x n) (cons (cons x x) out)) null this)))



; compile the talkback:
(let*((symbols (symbols-of get-main-entry))
      (codes   (codes-of   get-main-entry))
      (entry   (get-main-entry symbols codes))
      (bytes (fasl-encode entry)))

   (let*((path "talkback")
         (port (open-output-file path)))

      (write-bytes port bytes)
      (close-port port))

   (display "unsigned char *talkback = (unsigned char*) \"")
   (for-each (lambda (x)
                (display "\\x")
                (display (string (ref "0123456789abcdef" (div x 16))))
                (display (string (ref "0123456789abcdef" (mod x 16)))))
      bytes)
   (display "\";")
)
