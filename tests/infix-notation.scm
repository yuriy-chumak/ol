#!/usr/bin/env ol

(define-macro M (lambda args
   (define priority {
      '+ 1 '- 1
      '* 2 '/ 2
   })
   ;; (define left-associative? {
   ;;    '* 1
   ;; })
   (define (operator? op)
      (has? '(+ - * /) op))

   (let* ((expr tail (letrec (
            (math (lambda (queue) ; expression handler
               (define-values (a queue) (walk queue))
               (let loop ((a a) (queue queue))
                  (cond
                     ((null? queue) ; only one argument
                        (values a queue))
                     ((and (pair? (car queue)) (eq? (caar queue) 'unquote)) ; end of argument
                        ; handle comma: (unquote g) (...) -> g (...)
                        (values a (cons (cadar queue) (cdr queue))))
                     (else
                        (define-values (op1 queue) (walk queue))
                        (assert (operator? op1))
                        (define-values (b queue) (walk queue))

                        ; only one operator
                        (if (null? queue)
                           (values (list op1 a b) queue)
                        else
                           (define-values (op2 queue) (walk queue))
                           (assert (operator? op2))

                           ; todo: специальный случай для правоассоциативных операций (чтобы умножение матриц работало правильно, например)
                           (if (> (priority op2) (priority op1))
                           then
                              (define-values (b queue) (loop b (cons op2 queue)))
                              (values (list op1 a b) queue)
                           else
                              (loop (list op1 a b) (cons op2 queue)) )))))))
            (walk (lambda (queue) ; arguments handler
               (define a (car queue))
               (cond
                  ((number? a)
                     (values a (cdr queue)))
                  ((operator? a)
                     (values a (cdr queue)))

                  ((pair? a) ; parentheses
                     (let*((a t (math a)))
                        (assert (null? t))
                        (values a (cdr queue))))
                  ((symbol? a) ; function? (todo: check list of functions, or do (function? (eval a)))
                     (cond
                        ((null? (cdr queue)) ; variable, not a function
                           (values a (cdr queue)))
                        ((and (pair? (cadr queue)) (eq? (caadr queue) 'unquote)) ; stop on comma
                           (values a (cdr queue)))
                        (else ; function()
                           (define args (cadr queue))
                           (if (null? args) ; no arguments
                              (values (list a) (cddr queue)) ; always cddr
                           else
                              (assert (pair? args))
                              (let* ((arg args (math args)))
                                 (values
                                    (cons a
                                       (let loop ((out (list arg)) (args args))
                                          (if (null? args)
                                             (reverse out)
                                          else
                                             (assert (pair? args))
                                             (let* ((arg args (math args)))
                                                (loop (cons arg out) args)))))
                                    (cddr queue))))))) ))) )
         (math args)) ))
      (assert (null? tail))
      expr) ))

(define (f x) (* x x))
(define (g x y) (* (f x) y))
(define x 3)

(print (M -3 + g(x, 2 + 1 * 3)))
