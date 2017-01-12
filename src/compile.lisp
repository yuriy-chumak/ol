(import (owl unicode))
(import (lang eval)
        (lang sexp)

        (lang env)
        (lang ast)
        (lang fixedpoint)
        (lang alpha)
        (lang cps)
        (lang closure)
        (lang compile)

        (lang macro)
        (lang thread)
        (lang primop)
        (owl intern))

(define syntax-error-mark (list 'syntax-error))

      (define (sys:read fd maxlen)         (syscall 0 fd maxlen #false))

      (define (try-get-block fd block-size block?)
         (let ((res (sys:read fd block-size)))
;            (print "res: " res ", block?: " block?)
            (if (eq? res #true) ;; would block
               (if block?
                  (begin
                     (display ".")
                     (syscall 1200 #f #f #f) ; wait for a moment
                     ;(interact sid 5) ; todo: add checking the existing of thread controller
                     (try-get-block fd block-size #true))
                  res)
               res))) ;; is #false, eof or bvec

      (define (take-nap)
         (syscall 1200 #f #f #f))


(define (syntax-fail pos info lst)
;   (print "error")
   (list syntax-error-mark info
      (list ">>> " "x" " <<<")))

; -- parse --------------------------------------------------
      (define (assert pred val) ; fixme: should have a error message to throw when no luck
         (λ (ll ok fail pos)
            (let ((res (pred val)))
               (if res
                  (ok ll fail val pos)
                  (fail pos "parser assert blocked")))))


      (define-syntax let-parses
         (syntax-rules (verify eval)
            ((let-parses 42 sc ft lst pos ((val (eval term)) . r) body)
               (let ((val term))
                  (let-parses 42 sc ft lst pos r body)))
            ((let-parses 42 sc ft lst pos ((val parser) . r) body)
               (parser lst
                  (λ (lst ft val pos)
                     (let-parses 42 sc ft lst pos r body))
                  ft pos))
            ((let-parses 42 sc ft lst pos () body) (sc lst ft body pos))
            ((let-parses 42 sc ft lst pos ((verify term msg) . r) body)
               (if term
                  (let-parses 42 sc ft lst pos r body)
                  (ft pos msg)))
            ((let-parses ((a . b) ...) body)
               (λ (ll ok fail pos)
                  (let-parses 42 ok fail ll pos ((a . b) ...) body)))))

      (define (get-greedy parser zero-ok?)
         (λ (lst ok fail pos)
            (let loop ((lst lst) (rvals null) (pos pos))
               (parser lst
                  (λ (lst fail val pos)
                     (loop lst (cons val rvals) pos))
                  (λ (fpos freason)
                     (if (or zero-ok? (pair? rvals))
                        (ok lst fail (reverse rvals) pos) ; pass the original failure cont
                        (fail fpos freason))) ; could fail differently when zero and requested at least one
                  pos))))

      (define (get-greedy* parser) (get-greedy parser #true))
      (define (get-greedy+ parser) (get-greedy parser #false))

      (define (get-either a b)
         (λ (lst ok fail pos)
            (a lst ok
               (λ (fa fai)
                  (b lst ok (λ (fb fbi) (if (< fa fb) (fail fb fbi) (fail fa fai))) pos))
               pos)))

      (define-syntax get-any-of
         (syntax-rules ()
            ((get-any-of a) a)
            ((get-any-of a b) (get-either a b))
            ((get-any-of a . bs)
               (get-either a (get-any-of . bs)))))

      (define eof-error "end of input")

      (define (get-byte ll ok fail pos)
         (cond
            ((null? ll) (fail pos eof-error)) ; will always be the largest value
            ((pair? ll) (ok (cdr ll) fail (car ll) (+ pos 1)))
            (else (get-byte (ll) ok fail pos))))

      ;; testing a slower one to check assertions
      (define (get-byte-if pred)
         (let-parses
            ((b get-byte)
             (verify (pred b) "bad byte")
             ;(_ (assert pred b))
             )
            b))

      (define (get-between below above)
         (get-byte-if
            (λ (x)
               (and (less? below x) (less? x above)))))
               
      (define (get-imm n)
         (let-parses
            ((a get-byte)
             (verify (eq? a n) '(expected n)))
            a))


      ; #b10xxxxxx
      (define get-extension-byte
         (let-parses
            ((b get-byte)
             (verify (eq? #b10000000 (vm:and b #b11000000)) "Bad extension byte"))
            b))

      ;; fixme: could also support the longer proposed ones
      ;; fixme: get-rune == get-utf-8
      (define get-rune
         (get-any-of
            (get-byte-if (λ (x) (less? x 128)))
            (let-parses
               ((a (get-between 127 224))
                (verify (not (eq? a #b11000000)) "blank leading 2-byte char") ;; would be non-minimal
                (b get-extension-byte))
               (two-byte-point a b))
            (let-parses
               ((a (get-between 223 240))
                (verify (not (eq? a #b11100000)) "blank leading 3-byte char") ;; would be non-minimal
                (b get-extension-byte) (c get-extension-byte))
               (three-byte-point a b c))
            (let-parses
               ((a (get-between 239 280))
                (verify (not (eq? a #b11110000)) "blank leading 4-byte char") ;; would be non-minimal
                (b get-extension-byte) (c get-extension-byte) (d get-extension-byte))
               (four-byte-point a b c d))))

      (define (get-rune-if pred)
         (let-parses
            ((rune get-rune)
             (rune (assert pred rune)))
            rune))

      ; rchunks fd block? -> rchunks' end?
      ;; bug: maybe-get-input should now use in-process mail queuing using return-mails interop at the end if necessary
      (define (maybe-get-input rchunks fd block? prompt)
         (let ((chunk (try-get-block fd 1024 #false)))
;            (print "chunk: " chunk ", rchunks: " rchunks)
            ;; handle received input
            (cond
               ((not chunk) ;; read error in port
                  (values rchunks #true))
               ((eq? chunk #true) ;; would block
                  (take-nap) ;; interact with sleeper thread to let cpu sleep
                  (values rchunks #false))
               ((eof? chunk) ;; normal end if input, no need to call me again
                  (values rchunks #true))
               (else
                  (maybe-get-input (cons chunk rchunks) fd #false prompt)))))

      (define (push-chunks data rchunks)
         (if (null? rchunks)
            data
            (append data
               (foldr append null
                  (map vec->list (reverse rchunks))))))

      (define (fd->exp-stream fd prompt parse fail re-entry?) ; re-entry? unused
         (let loop ((old-data null) (block? #true) (finished? #false)) ; old-data not successfullt parseable (apart from epsilon)
            ;(print "fd->exp-stream: " old-data)
            (lets
               ((rchunks end?
                  (if finished?
                     (values null #true)
                     (maybe-get-input null fd (or (null? old-data) block?)
                        (if (null? old-data) prompt "|   "))))
                (data (push-chunks old-data rchunks)))

               ;(print "rchunks: " rchunks ", " data)
               (if (null? data)
                  (if end? null (loop data #true #false))
                  (parse data
                     (λ (data-tail backtrack val pos)
                        (pair val
                           (if (and finished? (null? data-tail))
                              null
                              (loop data-tail (null? data-tail) end?))))
                     (λ (pos info)
                        (cond
                           (end?
                              ; parse failed and out of data -> must be a parse error, like unterminated string
                              (list (fail pos info data)))
                           ((= pos (length data))
                              ; parse error at eof and not all read -> get more data
                              (loop data #true end?))
                           (else
                              (list (fail pos info data)))))
                     0)))))

; ------------------------------------------------------------------------------

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

; ==============================================================================
(define parser
   (let-parses (
         (line (get-greedy+ (get-rune-if (lambda (x) (not (eq? x #\newline))))))
         (skip (get-imm #\newline)))
      (runes->string line)))

; ...
(define (sleep1) (syscall 1200 #f #f #f))

      (define (? x) #true)

      (define (repl-fail env reason) (tuple 'error reason env))
      (define (repl-ok env value) (tuple 'ok value env))

      (define (ok exp env) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))
      (define (ok? x) (eq? (ref x 1) 'ok))

      (define repl-message-tag "foo")
      (define (repl-message foo) (cons repl-message-tag foo))
      (define (repl-message? foo) (and (pair? foo) (eq? repl-message-tag (car foo))))

      (define definition?
         (let ((pat (list 'setq symbol? ?)))
            (λ (exp) (match pat exp))))

      (define multi-definition?
         (let ((pat (list 'setq list? ?)))
            (λ (exp) (match pat exp))))

      (define (syntax-error? x) (and (pair? x) (eq? syntax-error-mark (car x))))


      (define (maybe-name-function env name value)
         (if (function? value)
            (env-set env name-tag
               (put (env-get env name-tag empty) value name))
            env))


      ; ------

      (define (execute exp env)
         (apply-values (exp)
            (lambda vals
               (ok
                  (cond
                     ((null? vals) "no vals")
                     ((null? (cdr vals)) (car vals))
                     (else (cons 'values vals)))
                  env))))


      (define compiler-passes (list
         apply-env
         sexp->ast
         fix-points
         alpha-convert
         cps
         build-closures
         compile
         execute
      ))

      (define (evaluate-as exp env task-unused)
         ; run the compiler chain in a new task
         (let ((result
         (call/cc
            (λ (exit)
               (fold
                  (λ (state next)
                     (if (ok? state)
                        (begin
                           ;(print "* " (ref state 2))
                           (next (ref state 2) (ref state 3)))
                        (begin
                           ;(print "exit")
                           (exit state))))
                  (ok exp env)
                  compiler-passes)))))
            ;(print (ref result 1))
            result))

      (define (evaluate exp env)
         (evaluate-as exp env 'repl))


      (define (eval-repl exp env repl)
         ;(print "eval-repl: " exp)
         (tuple-case (macro-expand exp env)
            ((ok exp env)
               (cond
                  ((definition? exp)
                     ;(print "definition: " exp)
                     (tuple-case (evaluate (caddr exp) env)
                        ((ok value env2)
                           (lets
                              ((env (env-set env (cadr exp) value))
                               (env (maybe-name-function env (cadr exp) value))
                               ;(env (maybe-save-metadata env (cadr exp) value))
                               )
                              (ok
                                 (repl-message
                                    (bytes->string (render ";; Defined " (render (cadr exp) null))))
                                 (bind-toplevel env))))
                        ((fail reason)
                           (fail
                              (list "Definition of" (cadr exp) "failed because" reason)))))
                  ((multi-definition? exp)
                     ;(print "multi-definition: " exp)
                     (tuple-case (evaluate (caddr exp) env)
                        ((ok value env2)
                           (let ((names (cadr exp)))
                              (if (and (list? value)
                                    (= (length value) (length names)))
                                 (ok (repl-message ";; All defined")
                                    (fold
                                       (λ (env pair)
                                          (env-set env (car pair) (cdr pair)))
                                       env
                                       (zip cons names value)))
                                 (fail
                                    (list "Didn't get expected values for definition of " names)))))
                        ((fail reason)
                           (fail
                              (list "Definition of" (cadr exp) "failed because" reason)))))
                  (else
                     ;(print "evaluate: " exp)
                     (evaluate exp env))))
            ((fail reason)
               (tuple 'fail
                  (list "Macro expansion failed: " reason)))))

;------
(define (get-main-entry symbols codes)
   (let*((interner-thunk (initialize-interner symbols codes)))
      ; main: / entry point of the REPL
      (λ (vm-args)
         ;(print "//vm-args: " vm-args)
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
                        ;(set-signal-action repl-signal-handler)

                        ;; repl
                        (begin
                           (print "<pre>")

         (let loop ((env  (interaction-environment))
                    (in   (fd->exp-stream stdin "> " sexp-parser syntax-fail #false))
                    (last 'blank)) ; last - последний результат
            ;(print "loop:")
            (cond
               ((pair? in)
                  (lets ((this in (uncons in #false)))
                     (cond
                        ((eof? this)
                           ;(print "EOF")
                           (repl-ok env last))
                        ((syntax-error? this)
                           (print "SYNTAX-ERROR")
                           (repl-fail env (cons "This makes no sense: " (cdr this))))
                        (else
                           (tuple-case (eval-repl this env repl)
                              ((ok result env)
                                 ;(print "OK!!!")
                                 (loop env in result))
                              ((fail reason)
                                 (print "FAIL!!!")
                                 (repl-fail env reason)))))))
               (else
                  (loop env (in) last)))))
                             ;; repl end
                              
                              )))))
            null)))) ; no threads state

; run
;(define main (lambda (args)

(let*((symbols (symbols-of get-main-entry))
      (codes   (codes-of   get-main-entry))
      (entry   (get-main-entry symbols codes))
      (bytes (fasl-encode entry)))

   (let*((path "program.b")
         (port (open-output-file path)))

      (write-bytes port bytes)
      (close-port port))

   (display "unsigned char *language = (unsigned char*) \"")
   (for-each (lambda (x)
                (display "\\x")
                (display (string (ref "0123456789abcdef" (div x 16))))
                (display (string (ref "0123456789abcdef" (mod x 16)))))
      bytes)
   (display "\";"))


;
;(let*((path "program.b")
;      (port (open-output-file path))
;
;      (bytes (fasl-encode main)))
;   (write-bytes port bytes)
;   (close-port port))
