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
,forget-all-but (*vm-special-ops* *libraries* *codes* wait stdin stdout stderr set-ticker-value build-start)


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
(import (owl sexp))

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

(import (lang ast))
(import (lang fixedpoint))
(import (lang cps))
(import (lang alpha))

; a value that can be created by an instruction

(define (small-value? val)
   (or
      (and (fixnum? val) (>= val -127) (< val 127))   
      (eq? val #true)
      (eq? val #false)
      (eq? val null)))

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

(define file-in 0)
(define file-out 1)

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

;,load "owl/arguments.scm"
;,load "owl/random.scm"

(import (owl random))

(import (owl args))

;(import (lang cgen))

(import (only (lang dump) make-compiler dump-fasl load-fasl))

; create the compiler:
(define compiler ; <- to compile things out of the currently running repl using the freshly loaded compiler
   (make-compiler *vm-special-ops*))

; path -> 'loaded | 'saved
(define (suspend path)
   (let ((maybe-world (wrap-the-whole-world-to-a-thunk #true #true)))
      (if (eq? maybe-world 'resumed)
         'loaded
         (begin
            (dump-fasl maybe-world path)
            'saved))))

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
         (system-stderr "Failed to enter seccomp sandbox. 
You must be on a newish Linux and have seccomp support enabled in kernel.
")
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
(import (owl pinvoke))

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


;,load "owl/test.scm"     ; a simple algorithm equality/benchmark tester
;,load "owl/sys.scm"      ; more operating system interface

(define shared-bindings shared-misc)

(define initial-environment-sans-macros
   (fold 
      (λ (env pair) (env-put-raw env (car pair) (cdr pair)))
      *owl-core*
      shared-bindings))
     
;; owl core needed before eval

;; toplevel can be defined later


(define initial-environment
   (bind-toplevel
      (library-import initial-environment-sans-macros
         '((owl base))
         (λ (reason) (error "bootstrap import error: " reason))
         (λ (env exp) (error "bootstrap import requires repl: " exp)))))

;; todo: after there are a few more compiler options than one, start using -On mapped to predefined --compiler-flags foo=bar:baz=quux

;; repl-start, thread controller is now runnig and io can be 
;; performed. check the vm args what should be done and act 
;; accordingly.

; note, return value is not the owl return value. it comes
; from thread controller after all threads have finished.


(define (strip-zeros n)
   (cond
      ((= n 0) n)
      ((= 0 (rem n 10))
         (strip-zeros (div n 10)))
      (else n)))

(define (memory-limit-ok? n w)
   (cond
      ((< n 1) (print "Too little memory allowed.") #false)
      ((and (= w 4) (> n 4096)) (print "This is a 32-bit executable, so you cannot use more than 4096Mb of memory.") #false)
      ((and (= w 8) (> n 65536)) (print "65536 is as high as you can go.") #false)
      (else #true)))

(define (maybe-set-memory-limit args)
   (let ((limit (get args 'memlimit #false)))
      (if limit
         (if (memory-limit-ok? limit (get-word-size))
            (set-memory-limit limit)
            (system-println "Bad memory limit")))))

(define (c-source-name path)
   (cond
      ((m/\.[a-z]+$/ path) ;; .scm, .lisp, .owl etc
         (s/\.[a-z]+$/.c/ path))
      (else
         (string-append path ".c"))))

(define (try thunk fail-val)
   ; run the compiler chain in a new task
   (let ((id (list 'thread)))
      (fork-linked-server id thunk)
      (tuple-case (ref (accept-mail (λ (env) (eq? (ref env 1) id))) 2)
         ((finished result not used)
            result)
         ((crashed opcode a b)
            (print-to stderr (verbose-vm-error opcode a b))
            fail-val)
         ((error cont reason info)
            ; note, these could easily be made resumable by storing cont
            (print-to stderr
               (list->string
                  (foldr render '(10) (list "error: " reason info))))
            fail-val)
         (else is bad ;; should not happen
            (print-to stderr (list "que? " bad))
            fail-val))))

(define (owl-run outcome args path profile?)
   (if outcome
      (tuple-case outcome
         ((ok val env)
            ;; be silent when all is ok
            ;; exit with 127 and have error message go to stderr when the run crashes
            (if profile?
               (try (λ () (profile (λ () (val args)) 30)) 127)
               (try (λ () (val args)) 127)))
         ((error reason env)
            (print-repl-error
               (list "ol: cannot run" path "because there was an error during loading:" reason))
            2))
      1))

;;;
;;; MCP, master control program and the thread controller
;;;

; special keys in mcp state 

;; pick usual suspects in a module to avoid bringing them to toplevel here
;; mainly to avoid accidentalaly introducing bringing generic functions here

(define-library (owl usuals)
   (export usual-suspects)
   ; make sure the same bindings are visible that will be at the toplevel

   (import (r5rs base))
   (import
      (owl math)
      (owl random)
      (lang thread)
      (owl list)
      (owl list-extra)
      (owl interop)
      (owl vector)
      (owl sort)
      (owl equal)
      (owl ff)
      (owl pinvoke)
      (owl sexp))

   (begin
      ; commonly needed functions 
      (define usual-suspects
         (list
            put get del ff-fold fupd
            - + * /
            div gcd ediv
            << < <= = >= > >> 
            equal? has? mem
            band bor bxor
            sort
            ; suffix-array bisect
            fold foldr for map reverse length zip append unfold
            lref lset iota
            ;vec-ref vec-len vec-fold vec-foldr
            ;print 
            mail interact 
            take keep remove 
            thread-controller
            ;sexp-parser 
            dlopen dlsym RTLD_LAZY
            ))))
(import (owl usuals))

;; say hi if interactive mode and fail if cannot do so (the rest are done using 
;; repl-prompt. this should too, actually)
(define (greeting seccomp?)
   (if (syscall 16 stdin 19 #f)
      (and
         (print (if seccomp? owl-ohai-seccomp owl-ohai))
         (display "> "))))


; *owl* points to owl root directory
; initally read from binary path (argv [0] )

(define (directory-of path)
   (runes->string
      (reverse
         (drop-while 
            (lambda (x) (not (eq? x #\/))) ; 47
            (reverse
               (string->bytes path))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Dump a new repl image
;;;

(define (heap-entry symbol-list)
   (lambda (codes) ;; all my codes are belong to codes
      (let*
         ((initial-names *owl-names*)
          (interner-thunk (initialize-interner symbol-list codes)))
         (λ (vm-special-ops)
            (let ((compiler (make-compiler vm-special-ops)))
               ;; still running in the boostrapping system
               ;; the next value after evaluation will be the new repl heap
               ;; start point for the vm

               ;; entry point of the compiled image?
               (λ (vm-args)
                  ;(print "vm-args: " (null? vm-args "null" vm-args))
                  ;; now we're running in the new repl 
                  (start-thread-controller
                     (list
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

                                 (exit-owl
                                    (let ((seccomp? #false)) ; else (seccomp megs)
                                       (greeting seccomp?)
                                       (repl-trampoline repl
                                          (fold ; this is our environment:
                                             (λ (env defn)
                                                (env-set env (car defn) (cdr defn)))
                                             initial-environment
                                             (list
                                                ;(cons '*owl* (directory-of (car vm-args)))
                                                (cons 'dump compiler)
                                                (cons 'eval exported-eval)
                                                (cons 'render render) ;; can be removed when all rendering is done via libraries
                                                ; globals
                                                (cons '*owl-version* *owl-version*)
                                                ;;(cons '*owl-metadata* *owl-metadata*)
                                                (cons '*owl-names* initial-names)
                                                (cons '*vm-args* vm-args)
                                                (cons '*vm-special-ops* vm-special-ops)
                                                (cons '*seccomp* seccomp?)
                                                ;;(cons '*codes* (vm-special-ops->codes vm-special-ops))
                                                ))))))))))
                     null)))))))

;; todo: dumping with fasl option should only dump the fasl and only fasl


;;;
;;; Dump the new repl
;;;

;; note, one one could use the compiler of the currently running system, but using 
;; the rebuilt one here to make changes possible in 1 instead of 2 build cycles.
;; (this may be changed later)
(print "Code loaded at " (- (time-ms) build-start) " ms.")
(print "Compiling ...")

(compiler heap-entry "unused historical thingy"
   (list->ff
     `((output . "boot.fasl")      ; output file
       (want-symbols . #true)      ;?
;       (want-threads . #true)
       (want-codes . #true)        ;?
       (want-native-ops . #true))) ;?
   "some") ; "none" = null, "some" = usual-suspects, "all" = heap-entry : vm extensions (none, some, all)
(print "Output written at " (- (time-ms) build-start) " ms.")
