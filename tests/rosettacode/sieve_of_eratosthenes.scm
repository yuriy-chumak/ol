#!/usr/bin/ol

(print
   (let loop ((left #null) (right (cdr (iota 1000 1))))
      (if (null? right)
         (reverse left)
         (loop
            (cons (car right) left)
            (filter (lambda (x) (not (eq? (mod x (car right)) 0))) (cdr right))))))
