(define-macro (define-sorted x array)
   (begin
      (define (bubble-sort x ??)
         (define (sort-step l)
            (if (or (null? l) (null? (cdr l)))
               l
               (if (?? (car l) (cadr l))
                  (cons (cadr l) (sort-step (cons (car l) (cddr l))))
                  (cons (car  l) (sort-step (cdr l))))))
         (let loop ((i x))
            (define step (sort-step i))
            (if (equal? i step)
               i
               (loop step))))
      `(define x (quote ,(bubble-sort (cadr array) >)))))

(define-sorted x '(2 10 4 5 3 19 12 8 18 20 1 11))
(print "x: " x)
