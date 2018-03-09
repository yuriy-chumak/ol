;;;
;;; ol.scm: an Otus Lisp read-eval-print loop (REPL) binary image compiler.
;;;

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

(print "Loading code...")
(define build-start (time-ms))

; forget all other libraries to have them be reloaded and rebuilt
; виртуальная (src olvm) содержит список базовых элементов языка
(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(src olvm)))
      *libraries*))

; предварительная загрузка зависимостей r5rs core,
; (иначе импорт сбойнет)
(import (src vm))   ;; команды виртуальной машины
(import (r5rs srfi-16))   ;; case-lambda for core
(import (r5rs srfi-87))   ;; "=>" clauses in case
(import (r5rs core)) ;; базовый языковый набор ol

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *codes* *vm-args* wait stdin stdout stderr set-ticker-value build-start)


;;;
;;; Time for a new REPL
;;;
(import (src olvm))     ;; get special forms, primops and define-syntax (virtual library)

;; this should later be just a sequence of imports followed by a fasl dump
(import (r5rs core))    ;; get define, define-library, import, ... from the just loaded

(define *include-dirs* '("." "libraries")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)


(define *loaded* '())   ;; can be removed soon, was used by old ,load and ,require


;; shared parameters, librarize later or remove if possible

;; http://semver.org/lang/ru/
(define *owl-version* "1.1")
(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it
(define max-object-size #xffff)  ; todo: change as dependent of word size


(import (otus lisp))

(import (lang intern))
(import (lang threading))

(import (owl parse))

(import (lang gensym))
(import (lang env))
(import (lang macro))
(import (lang sexp))

(import (lang ast))
(import (lang fixedpoint))
(import (lang alpha))
(import (lang cps))
(import (lang closure))
(import (lang assemble))
(import (lang compile))



(define error-tag "err")
(define (error? x)
   (and (tuple? x)
      (eq? (ref x 1) error-tag)))

(import (owl time))
(import (owl fasl))

;; fixme: should sleep one round to get a timing, and then use avg of the last one(s) to make an educated guess
;(define (sleep ms)
;   (lets ((end (+ ms (time-ms))))
;      (let loop ()
;         ;(print (interop 18 1 1))
;         (let ((now (time-ms)))
;            (if (> now end)
;               now
;               (begin (interact sleeper-id 50) (loop)))))))

; -> mcp gets <cont> 5 reason info
; (run <mcp-cont> thunk quantum) -> result

;(define input-chunk-size  1024)
;(define output-chunk-size 4096)

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

;;;
;;; Entering sandbox
;;;

;; a temporary O(n) way to get some space in the heap

;; fixme: allow a faster way to allocate memory
;; n-megs → _
(define (ensure-free-heap-space megs)
   #true)
;   (if (> megs 0)
;      (lets
;         ((my-word-size (get-word-size)) ;; word size in bytes in the current binary (4 or 8)
;          (blocksize 65536)              ;; want this many bytes per node in list
;          (pairsize (* my-word-size 3))  ;; size of cons cell, being [header] [car-field] [cdr-field]
;          (bytes                         ;; want n bytes after vector header and pair node for each block
;            (map (λ (x) 0)
;               (iota 0 1
;                  (- blocksize (+ pairsize my-word-size)))))
;          (n-blocks
;            (ceil (/ (* megs (* 1024 1024)) blocksize))))
;         ;; make a big data structure
;         (map
;            (λ (node)
;               ;; make a freshly allocated byte vector at each node
;               (list->byte-vector bytes))
;            (iota 0 1 n-blocks))
;         ;; leave it as garbage
;         #true)))
;
;; enter sandbox with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (sandbox n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or
      (syscall 157 #false #false #false)
      (begin
         (system-stderr "Failed to enter sandbox. \nYou must be on a newish Linux and have seccomp support enabled in kernel.\n")
         (halt exit-seccomp-failed))))


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

(print "Code loaded at " (- (time-ms) build-start) " ms.")

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

;
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
                     (fork-server 'repl (lambda ()
                        ;; get basic io running
                        (io:init)

                        ;; repl needs symbol etc interning, which is handled by this thread
                        (fork-intern-interner symbols)
                        (fork-bytecode-interner codes)

                        ;; set a signal handler which stop evaluation instead of owl
                        ;; if a repl eval thread is running
                        (set-signal-action repl-signal-handler)

                        ;; repl
                        (exit-owl
                           (let*((file (if (null? vm-args)
                                          stdin
                                          (if (string-eq? (car vm-args) "-")
                                             stdin
                                             (open-input-file (car vm-args)))))
                                 (options
                                    (let loop ((options #empty) (args (if (null? vm-args) null (cdr vm-args))))
                                       (cond
                                          ((null? args)
                                             options)
                                          ((string-eq? (car args) "--sandbox")
                                             (loop (put options 'sandbox #t) (cdr args)))
                                          ((string-eq? (car args) "--interactive")
                                             (loop (put options 'interactive #t) (cdr args)))
                                          ((string-eq? (car args) "--home") ; TBD
                                             (if (null? (cdr args))
                                                (runtime-error "no heap size in command line" args))
                                             (loop (put options 'home (cadr args)) (cddr args)))
                                          (else
                                             (loop options (cdr args))))))
                                 (home (or (getf options 'home)
                                           (getenv "OL_HOME")
                                           "/usr/lib/ol")) ; Linux, *BSD, etc.
                                 (sandbox? (getf options 'sandbox))
                                 (interactive? (or
                                           (getf options 'interactive)
                                           (syscall 16 file 19 #f))) ; isatty()

                                 (version (cons "OL" *version*))
                                 (env (fold
                                          (λ (env defn)
                                             (env-set env (car defn) (cdr defn)))
                                          initial-environment
                                          (list
                                             (cons '*owl-names*   initial-names)
                                             (cons '*owl-version* initial-version)
                                             (cons '*include-dirs* (list "." home))
                                             (cons '*interactive* interactive?)
                                             (cons '*vm-args* vm-args)
                                             (cons '*version* version)
                                            ;(cons '*scheme* 'r5rs)
                                             (cons '*sandbox* sandbox?)
                                          ))))
                              (if sandbox?
                                 (sandbox 1)) ;(sandbox megs) - check is memory enough
                              (repl-trampoline env file))))))))
            )))) ; no threads state



;(define symbols (symbols-of get-main-entry))
;(define codes   (codes-of   get-main-entry))

;;;
;;; Dump the new repl
;;;

(print "Compiling ...")

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
         ((vm:raw? node)
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




(let*((path "boot.fasl")
      (port ;; where to save the result
         (open-output-file path))

      (symbols (symbols-of get-main-entry))
      (codes   (codes-of   get-main-entry))
      (bytes ;; encode the resulting object for saving in some form
         (fasl-encode (get-main-entry symbols codes))))
   (if (not port)
      (begin
         (print "Could not open " path " for writing")
         #false)
      (begin ;; just save the fasl dump
         (write-bytes port bytes)
         (close-port port)
         (print "Output written at " (- (time-ms) build-start) " ms.")
         #true)))
