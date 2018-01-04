(define-library (owl equal)

   (import
      (r5rs core)
      (owl string)
      (owl symbol)
      (owl list)
      (owl math))

   (export
      equal?
      eqv?)

(begin
      (define (recursive-eq a b eq pos)
         (cond
            ((eq? pos 0)
               #true)
            ((eq (ref a pos) (ref b pos))
               (lets ((pos x (vm:sub pos 1)))
                  (recursive-eq a b eq pos)))))

      ;; fixme: ff:s should have a separate equality test too
      ;; fixme: byte vector paddings not here

      ;; raw brute force object equality
      (define (equal? a b)
         (cond
            ((eq? a b)
               #true)
            ((string? a)
               (and (string? b) (string-eq? a b)))
            ((symbol? a) #false) ; would have been eq?, because they are interned
            ((pair? a)
               (if (pair? b)
                  (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))
            (else
               (let ((sa (size a)))
                  (cond
                     ; a is immediate -> would have been eq?
                     ((not sa) #false)
                     ; same size
                     ((eq? sa (size b))
                        (let ((ta (type a)))
                           ; check equal types
                           (if (eq? ta (type b))
                              (recursive-eq a b equal? sa)))))))))

   (define ≡ equal?)
   (define eqv? equal?) ; todo: shoule not compare the nestede lists!!!
))
