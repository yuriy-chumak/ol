#!/usr/bin/env ol

; this test showing optimization of the lisp code

(define append1
   (letrec ((app (lambda (a b)
               (if (null? a)
                  b
                  (cons (car a) (app (cdr a) b)))))
            (appl (lambda (l)
               (if (null? (cdr l))
                  (car l)
                  (app (car l) (appl (cdr l)))))))
      (case-lambda
         ((a b) (app a b))
         ((a b . cs) (app a (app b (appl cs))))
         ((a) a)
         (() '()))))

(define append2
   (let*((app (lambda (a b app)
            (if (null? a)
               b
               (cons (car a) (app (cdr a) b app)))))
         (appl (lambda (l appl)
            (if (null? (cdr l))
               (car l) ; don't recurse down the list just to append nothing
               (app (car l) (appl (cdr l) appl) app)))))
   (case-lambda
      ((a b) (app a b app))
      ((a b . cs) (app a (app b (appl cs appl) app) app))
      ((a) a)
      (() '()))))

(assert (equal?
   (fasl-encode append1)
   (fasl-encode append2)))
