;;;
;;; ol.scm: an Owl read-eval-print loop binary image compiler.
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

(define *libraries*
   (keep
      (λ (lib)
         (equal? (car lib) '(owl core)))
      *libraries*))

(import (r5rs base)) ;; reload default macros needed for defining libraries etc

;; forget everhything except these and core values (later list also them explicitly)
,forget-all-but (*libraries* *codes* wait stdin stdout stderr set-ticker-value build-start)


;;;
;;; Time for a new REPL
;;;
(import (owl core))     ;; get special forms, primops and define-syntax

;; this should later be just a sequence of imports followed by a fasl dump
(import (r5rs base))    ;; get define, define-library, import, ... from the just loaded

(define *include-dirs* (list "." "/usr/lib/ol")) ;; now we can (import <libname>) and have them be autoloaded to current repl
(define *owl-names* #empty)

(import (owl interop))
(import (owl primop))

(define *loaded* '())   ;; can be removed soon, was used by old ,load and ,require


;; shared parameters, librarize later or remove if possible

;; http://semver.org/lang/ru/
(define *owl-version* "1.0.0")
(define exit-seccomp-failed 2)   ;; --seccomp given but cannot do it
(define max-object-size #xffff)  ; todo: change as dependent of word size

(define owl-ohai "You see a prompt.") ; todo: change to version string
(define owl-ohai-seccomp "You see a prompt. You feel restricted.")

;; throw an error if some familiar but unsupported Scheme functions are called
(define-library (owl unsupported)
   (export string-set! vector-set!) ; set! set-car! set-cdr! 

   (import 
      (r5rs base)
      (owl error)
      (owl interop))

   (begin
;      (define-syntax set!
;         (syntax-rules () 
;            ((set! var val) (error "set! is not supported: " '(set! var val)))))
;  
      (define (unsupported name)
         (error "Mutator not supported: " name))

;      (define (set-car! pair val) (unsupported "set-car!"))
;      (define (set-cdr! pair val) (unsupported "set-cdr!"))
      (define (vector-set! vec pos val) (unsupported "vector-set!"))
      (define (string-set! str pos val) (unsupported "string-set!"))))


;; move these simple ones to a separate library later (owl immediate?)
(import (owl list))
(import (owl ff))
(import (only (owl iff))) ;; hack, load it but don't import anything
(import (owl math))
(import (owl list-extra))
(import (owl sort))
(import (owl math-extra))
(import (owl lazy))
(import (only (owl unicode) encode-point))
(import (owl string))


;; move these elsewhere
(define (number->string n base)
   (list->string (render-number n null base)))

(import (owl vector))
(import (owl symbol))
(import (owl tuple))
(import (owl equal))
(import (owl rlist))

; todo: change to (eof-object? ) as "r5rs/6.6.2 Input", add #eof to parser
(define-library (owl eof)
   (export eof?)
   (import (r5rs base))
   (begin
      (define eof-value
         (cast 4 13))
      (define (eof? x) 
         ;(eq? type-eof (type x))
         (eq? x eof-value))))

(import (owl render))
(import (only (owl queue))) ; just load it
(import (owl intern))
(import (owl eof))
(import (owl io))
(import (owl parse))
(import (owl regex))

(define (ok? x) (eq? (ref x 1) 'ok))
(define (ok exp env) (tuple 'ok exp env))
(define (fail reason) (tuple 'fail reason))

(import (scheme misc))
(import (owl gensym))


;; does not belong here, but needed in macros for now 

(define (verbose-vm-error opcode a b)
   (cond
      ((eq? opcode 256)
         ; fixme, add but got ...
         (list 'function b 'expected a 'arguments))
      ((eq? opcode 52) (list "car: bad pair: " a))
      ((eq? opcode 53) (list "cdr: bad pair: " a))
      (else
         (list "error: " 'instruction opcode 'info (tuple a b)))))


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
   (if (> megs 0)
      (lets
         ((my-word-size (get-word-size)) ;; word size in bytes in the current binary (4 or 8)
          (blocksize 65536)              ;; want this many bytes per node in list
          (pairsize (* my-word-size 3))  ;; size of cons cell, being [header] [car-field] [cdr-field]
          (bytes                         ;; want n bytes after vector header and pair node for each block
            (map (λ (x) 0) 
               (iota 0 1 
                  (- blocksize (+ pairsize my-word-size)))))
          (n-blocks  
            (ceil (/ (* megs (* 1024 1024)) blocksize))))
         ;; make a big data structure
         (map
            (λ (node)
               ;; make a freshly allocated byte vector at each node
               (list->byte-vector bytes))
            (iota 0 1 n-blocks))
         ;; leave it as garbage
         #true)))

;; enter seccomp with at least n-megs of free space in heap, or stop the world (including all other threads and io)
(define (seccomp n-megs)
   ;; grow some heap space work working, which is usually necessary given that we can't 
   ;; get any more memory after entering seccomp
   (if (and n-megs (> n-megs 0))
      (ensure-free-heap-space n-megs))
   (or (sys-prim 1010 #false #false #false)
      (begin
         (system-stderr "Failed to enter seccomp sandbox. \nYou must be on a newish Linux and have seccomp support enabled in kernel.\n")
         (halt exit-seccomp-failed))))


(define-library (owl char)
   (export char->integer integer->char)
   (import (r5rs base))
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

(import (owl base))

;; push it to libraries for sharing, replacing the old one
(define *libraries* 
   (cons 
      (cons '(owl core) *owl-core*)
      (keep (λ (x) (not (equal? (car x) '(owl core)))) *libraries*)))

;; todo: share the modules instead later
(define shared-misc
   (share-bindings
;      error
;      boolean?  fixnum?  eof?  symbol?
;      tuple?  string?  function? procedure? equal? eqv? bytecode?
;      not
;      null?  null 
;      time
;      time-ms
;      halt exec
;      seccomp
;      apply
;      call/cc
;      call-with-current-continuation
;      display print-to print print* 
;      render 
;      system-println
;      sleep
;      list->tuple
;      exit-thread
;      number->string
;      fork
;      fork-named
;      fork-linked
;      fork-server
;      fork-linked-server
;      exit-owl
;      single-thread?
;      set-ticker-value
;      kill
;      catch-thread
;      release-thread
;      suspend
;      mail interact
;      string->number
;      wait
;      wait-mail accept-mail check-mail return-mails
;      set-signal-action
;      byte-vector?
;      string->symbol
;      close-port flush-port
;      ;dlopen dlsym RTLD_LAZY
;      set-memory-limit 
;      get-word-size
;      get-memory-limit
;      string->sexp
;      profile
      *features*
      *include-dirs*
      *libraries*      ;; all currently loaded libraries
      ))

(print "Code loaded at " (- (time-ms) build-start) " ms.")

;;;
;;; MCP, master control program and the thread controller
;;;

(define shared-bindings shared-misc)

(define initial-environment-sans-macros
   (fold 
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *owl-core*
      shared-bindings))

(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((owl base))
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
                           (let* ((isatty? (syscall 16 stdin 19 #f))
                                  (seccomp? #false) ; else (seccomp megs) - check is memory enough
                                  (env (fold
                                          (λ (env defn)
                                             (env-set env (car defn) (cdr defn)))
                                          initial-environment
                                          (list
                                             (cons '*owl-names*   initial-names)
                                             (cons '*owl-version* initial-version)
                                             (cons '*vm-args* vm-args)
                                             (cons '*seccomp* seccomp?)
                                          ))))
                              (if isatty?
                                 (begin ;greeting
                                    (print (if seccomp? owl-ohai-seccomp owl-ohai))
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
         ((immediate? node) trail)
         ((get trail node #false) trail)
         ((symbol? node) 
            (let ((trail (put trail node 1)))
               (put trail tag 
                  (cons node (get trail tag null)))))
         ((raw? node) trail)
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
      ((immediate? obj) (values seen empty))
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
