;;;
;;; ol.scm: an Otus Lisp read-eval-print loop (REPL) binary image compiler.
;;;

#| Copyright (c) 2012 Aki Helin
 | Copyright (c) 2014, 2015 Yuriy Chumak
 |
 | Permission is hereby granted, free of charge, to any person obtaining a
 | copy of this software and associated documentation files (the "Software"),
 | to deal in the Software without restriction, including without limitation
 | the rights to use, copy, modify, merge, publish, distribute, sublicense,
 | and/or sell copies of the Software, and to permit persons to whom the
 | Software is furnished to do so, subject to the following conditions
 |
 | The above copyright notice and this permission notice shall be included
 | in all copies or substantial portions of the Software.
 |
 | THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 | IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 | FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 | THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 | LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 | FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 | DEALINGS IN THE SOFTWARE.
 |#

(define build-start (time-ms))
(print "Loading code...")

(mail 'intern (tuple 'flush)) ;; ask intern to forget all symbols it knows

; forget all other libraries to have them be reloaded and rebuilt
; (src olvm) содержит список базовых элементов языка
(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(src olvm)))
      *libraries*))

(import (r5rs core)) ;; reload default macros needed for defining libraries etc

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *codes* wait stdin stdout stderr set-ticker-value build-start)


;;;
;;; Time for a new REPL
;;;
(import (src olvm))     ;; get special forms, primops and define-syntax

;; this should later be just a sequence of imports followed by a fasl dump
(import (r5rs core))    ;; get define, define-library, import, ... from the just loaded

(define *include-dirs* '(".")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)


(define *loaded* '())   ;; can be removed soon, was used by old ,load and ,require


;; shared parameters, librarize later or remove if possible

;; http://semver.org/lang/ru/
(define *owl-version* "1.0.0")
(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it
(define max-object-size #xffff)  ; todo: change as dependent of word size

(define owl-ohai "You see a prompt.") ; todo: change to version string
(define owl-ohai-seccomp "You see a prompt. You feel restricted.")


(import (otus lisp))

(import (owl primop))

(import (owl intern))
(import (owl parse))

(import (owl gensym))


(import (lang env))
(import (lang macro))
(import (lang sexp))

(import (lang ast))
(import (lang fixedpoint))
(import (lang cps))
(import (lang alpha))

(import (lang thread))
;import (lang assemble))
(import (lang closure))
(import (lang compile))


(define error-tag "err")

(define (error? x)
   (and (tuple? x)
      (eq? (ref x 1) error-tag)))

(import (owl time))
(import (owl fasl))

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

;;;
;;; Entering seccomp
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
;; enter seccomp with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (seccomp n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or
      (syscall 157 #false #false #false)
      (begin
         (system-stderr "Failed to enter seccomp sandbox. \nYou must be on a newish Linux and have seccomp support enabled in kernel.\n")
         (halt exit-seccomp-failed))))


(define-library (owl char)
   (export char->integer integer->char)
   (import (r5rs core))
   (import
      (owl math))
   (begin
      (define self (λ (x) x))

      (define char->integer self)
      (define integer->char self)))

;; profiling doesn't yet have a good home. merge to lib-internals or lib-debug later?
;; run thunk and show n most called functions. no timings yet.
(define (profile thunk n)
   (lets
      ((skip (start-profiling))
       (skip (set-ticker-value 0))
       (res (thunk))
       (stats (stop-profiling))
       (most-used
         (take
            (sort
               (λ (a b) (> (car a) (car b)))
               (ff-fold
                  (λ (out func n)
                     (cons (cons n func) out))
                  null stats))
            n)))
      (for-each (λ (p) (print*-to stdout  (list (car p) ":" (cdr p)))) most-used) ;; <- could use stderr later
      res))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; new repl image
;;;

;; say hi if interactive mode and fail if cannot do so (the rest are done using
;; repl-prompt. this should too, actually)
(define (get-main-entry symbols codes)
   (let*((initial-names   *owl-names*)
         (initial-version *owl-version*)

         (interner-thunk (initialize-interner symbols codes)))
      ; main: / entry point of the compiled image
      (λ (vm-args)
         ;(print "vm-args: " (null? vm-args "null" vm-args))
         ;; now we're running in the new repl
         (start-thread-controller
            (list ;1 thread
               (tuple 'init
                  (λ ()
                     (fork-server 'repl (lambda ()
                        ;; get basic io running
                        (start-base-threads)

                        ;; repl needs symbol etc interning, which is handled by this thread
                        (fork-server 'intern interner-thunk)

                        ;; set a signal handler which stop evaluation instead of owl
                        ;; if a repl eval thread is running
                        (set-signal-action repl-signal-handler)

                        ;; repl
                        (exit-owl
                           (let*((seccomp? (if (pair? vm-args)
                                             (let loop ((args (cdr vm-args)))
                                                          (cond
                                                             ((null? args) #f)
                                                             ((string-eq? (car args) "--seccomp") #t)
                                                             (else (loop (cdr args)))))))
                                 (env (fold
                                          (λ (env defn)
                                             (env-set env (car defn) (cdr defn)))
                                          initial-environment
                                          (list
                                             (cons '*owl-names*   initial-names)
                                             (cons '*owl-version* initial-version)
                                             (let ((ol-home (getenv "OL_HOME")))
                                                (cons '*include-dirs* (list "." (if ol-home ol-home
                                                   (cond
                                                      ((string-eq? (ref (uname) 1) "Linux") "/usr/lib/ol")
                                                      ((string-eq? (ref (uname) 1) "Windows") "C:/Program Files/OL")
                                                      (else "."))))))
                                             (cons '*vm-args* vm-args)
                                             (cons '*version* (vm:version))
                                            ;(cons '*scheme* 'r5rs)
                                             (cons '*seccomp* seccomp?)
                                          ))))
                              (if seccomp?
                                 (seccomp 1)) ;(seccomp megs) - check is memory enough
                              (if (syscall 16 stdin 19 #f) ; isatty?
                                 (begin ;greeting
                                    (print (if seccomp? owl-ohai-seccomp owl-ohai))
                                    (print "Type ',help' to help, ',quit' to end session")
                                    (display "> ")))
                              (repl-trampoline repl env))))))))
            null)))) ; no threads state



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
