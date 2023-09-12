(define-library (math infix-notation)
   (version 1.0)
   (license MIT/LGPL3)
(import
   (scheme core)
   (scheme list))

(export
   infix-notation \\

   ; infix notation options
   \\operators
   \\right-operators
   \\postfix-functions
)

(begin
   (import (owl ff))
   ; todo: add unary functions (like -)
   ; todo: add ∧,or, xor

   (define \\operators {
      '+ 2 '- 2 '− 3
      '* 3 '/ 3
      '· 3 '× 3 ; dot and cross
      '^ 4 ; power

      ; additional synonyms
      ': 3 '÷ 3 ; same as /
      '• 3 '⨯ 3 ; dot and cross
      '** 4 ; same as power
   })
   ; todo: <<, >>

   (define \\right-operators {
      '^ #t ; power
      '** #t ; same as power
   })

   (define \\postfix-functions {
      '! #t
      ; additional unicode synonyms
      '¹ #t  '² #t  '³ #t  '⁴ #t  '⁵ #t
      'ᵀ #t
   })

   (define-macro infix-notation (lambda args
      (define priority \\operators)
      (define (operator? op)
         (\\operators op #false))

      ; без правой ассоциативности мы никуда,
      ; иначе матричная арифметика, например,
      ; может стать очень тяжелой.
      (define (right-operator? op)
         (\\right-operators op #false))
      (define (left-operator? op)
         (and (operator? op) (not (right-operator? op))))

      (define (postfix-function? op)
         (\\postfix-functions op #false))

      ;..
      ; infix -> postfix
      ; в стеке у нас только операторы, функции и левая скобка (в виде символа #eof)
      ; https://en.wikipedia.org/wiki/Shunting_yard_algorithm
      (define-values (stack out)
      (letrec ((braces (lambda (queue stack out)
                  ;; (print "braces queue: " queue)
                  (if (and (pair? queue) (eq? (car queue) '-))
                     (braces (cons 0 queue) stack out)  ;(braces (cons 'negate (list (cdr queue))) stack out)
                     (continue queue stack out))))
               (continue (lambda (queue stack out)
                  ;; (print "queue: " queue)
                  (if (null? queue)
                  then
                     ; rollback the stack
                     (define-values (newstack newout)
                        (let subloop ((stack stack) (out out))
                           (if (null? stack)
                              (runtime-error "mismatched parentheses" args)
                           else
                              (define top (car stack))
                              (if (eof? top)
                                 (values (cdr stack) out) ; discard "("
                              else
                                 (subloop (cdr stack) (cons top out))))))

                     (if (null? newstack)
                        (values newstack newout)
                     else
                        (define top (car newstack))
                        (if (pair? top) ; prefix function?
                           (values (cdr newstack) (cons top newout))
                           (values newstack newout)))
                  ; process next token
                  else
                     (define token (car queue))
                     (cond
                        ((pair? token) ; (...)
                           (define-values (newstack newout) (braces token (cons #eof stack) out))
                           (continue (cdr queue) newstack newout))

                        ((null? token) ; ()
                           (define-values (newstack newout) (braces token (cons #eof stack) out))
                           (continue (cdr queue) newstack newout))

                        ((number? token)
                           (continue (cdr queue) stack (cons token out)))
                        ((string? token)
                           (continue (cdr queue) stack (cons token out)))

                        ((eq? token 'unquote)
                           (define-values (newstack newout)
                              (let subloop ((stack stack) (out out))
                                 (if (null? stack)
                                    stack
                                 else
                                    (define top (car stack))
                                    (if (eof? top)
                                       (values stack out)
                                    else
                                       (subloop (cdr stack) (cons top out))))))
                           (continue (cdr queue) newstack newout))

                        ((postfix-function? token)
                           (continue (cdr queue) stack (cons token out)))

                        ; todo: vector
                        ; todo: handle unary "-"
                        ((operator? token) ; binary operator
                           (define-values (newstack newout)
                              (let subloop ((stack stack) (out out))
                                 (if (null? stack)
                                    stack
                                 else
                                    (define top (car stack))
                                    (if (or
                                          (and
                                             (operator? top)
                                             (> (priority top 0) (priority token 0)))
                                          (and
                                             (left-operator? token)
                                             (= (priority top 0) (priority token 0))) )
                                    then
                                       (subloop (cdr stack) (cons top out))
                                    else
                                       (values stack out)))))
                           (continue (cdr queue) (cons token newstack) newout))

                        ; special macro (list ...)
                        ((eq? token 'list)
                           (continue (list (cons (cadr queue)
                                             (map (lambda (q) (list 'unquote q)) (cddr queue))))
                                 (cons (list token) stack) (cons #eof out)))

                        ((symbol? token) ; variable or function
                           (if (or
                                 (and (not (null? (cdr queue))) ; function with arguments
                                       (pair? (car (cdr queue)))
                                       (not (eq? (caadr queue) 'unquote)))
                                 (and (not (null? (cdr queue))) ; empty function
                                       (null? (car (cdr queue)))))
                           then ; function
                              (define arguments
                                 (let cycle ((in (reverse (cadr queue)))
                                             (cursor '())
                                             (out '()))
                                    (if (null? in)
                                       (if (null? cursor)
                                          out
                                          (cons cursor out))
                                    else
                                       (define x (car in))
                                       (if (and (pair? x)
                                                (eq? (car x) 'unquote))
                                          (if (null? cursor)
                                             (cycle (cdr in) '() (cons x out))
                                             (cycle (cdr in) '() (cons (cons* 'unquote (cadr x) cursor) out)))
                                       else
                                          (cycle (cdr in) (cons x cursor) out)))))
                              (continue (cons
                                       arguments
                                       (cddr queue))
                                    (cons (list token) stack) (cons #eof out))
                           else ; variable
                              (continue (cdr queue) stack (cons token out))))
                        (else
                           (print "error")))))))
         (braces args '(#eof) '())))

      (unless (null? stack)
         (runtime-error "invalid infix expression" args))
      ;; (print "out: " (reverse out))

      (define-values (in s-exp)
      (let loop ((in (reverse out)) (stack '()))
         ;; (print "stack: " stack)
         (if (null? in)
            (values in stack)
         else
            (define token (car in))
            (cond
               ((number? token)
                  (loop (cdr in) (cons token stack)))
               ((string? token)
                  (loop (cdr in) (cons token stack)))
               ((eof? token)
                  (define-values (in s-exp)
                     (loop (cdr in) '()))
                  (loop in (cons s-exp stack)))
               ((pair? token) ; function
                  (values (cdr in) (cons (car token) (reverse stack))))
               ((operator? token) ; binary operator
                  (loop (cdr in) (cons (list
                                          token
                                          (cadr stack) (car stack))
                                       (cddr stack))))
               ((postfix-function? token)
                  (loop (cdr in) (cons (list
                                          token
                                          (car stack))
                                       (cdr stack))))
               ((symbol? token)
                  (loop (cdr in) (cons token stack)))
               (else
                  (runtime-error "error" out))))))

      ;; (print "s-exp: " (car s-exp))
      (car s-exp)
   ))

   (define-macro \\ (lambda args
      (cons 'infix-notation args)))
))
