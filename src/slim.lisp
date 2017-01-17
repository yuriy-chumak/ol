(import (owl unicode))
(import (lang eval)
        (lang sexp)
        (lang assemble)

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
        (lang intern))
(import (owl parse))

; ------------------------------------------------------------------------------
(define syntax-error-mark (list 'syntax-error))

(define (syntax-fail pos info lst)
   (list syntax-error-mark info
      (list ">>> " "x" " <<<")))


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
(define (codes-of ob)

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

   (lets ((refs this (code-refs empty ob)))
      (ff-fold (λ (out x n) (cons (cons x x) out)) null this)))

; ==============================================================================
;(define parser
;   (let-parses (
;         (line (get-greedy+ (get-rune-if (lambda (x) (not (eq? x #\newline))))))
;         (skip (get-imm #\newline)))
;      (runes->string line)))
;
;(define (sleep1) (syscall 1200 #f #f #f))

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

      (define (evaluate-as exp env)
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
         (evaluate-as exp env))


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
   ; main: / entry point of the REPL
   (λ (vm-args)
      ;; now we're running in the new repl
      (start-thread-controller
         (list ;1 thread
            (tuple 'init
               (λ ()
                  (fork-server 'repl (lambda ()
                     ;; get basic io running
                     (start-base-threads)

                     ;; repl needs symbol etc interning, which is handled by this thread
                     (fork-intern-interner symbols)
                     (fork-bytecode-interner codes)

                     ;; repl
                     (let loop ((env  (interaction-environment))
                                (in   (lambda () (fd->exp-stream stdin "> " sexp-parser syntax-fail #false)))
                                (last 'blank)) ; last - последний результат
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
                                             (loop env in result))
                                          ((fail reason)
                                             (print "FAIL!!!")
                                             (repl-fail env reason)))))))
                           (else
                              (loop env (in) last))))
                     )))))
               null))) ; no threads state

; compile the web-repl:
(let*((symbols (symbols-of get-main-entry))
      (codes   (codes-of   get-main-entry))
      (entry   (get-main-entry symbols codes))
      (bytes (fasl-encode entry)))

;   (let*((path "repl-slim")
;         (port (open-output-file path)))
;
;      (write-bytes port bytes)
;      (close-port port))

   (display "unsigned char *language = (unsigned char*) \"")
   (for-each (lambda (x)
                (display "\\x")
                (display (string (ref "0123456789abcdef" (div x 16))))
                (display (string (ref "0123456789abcdef" (mod x 16)))))
      bytes)
   (display "\";")
)
