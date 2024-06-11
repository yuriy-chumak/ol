; otus lisp language
(define-library (otus lisp)

   (export
      (exports (scheme base))
      
      ; all Ol functions, not only (scheme base) profile:
      (exports (srfi 1))  ; * List Library
;      (exports (scheme r5rs iteration)) ; 4.2.4 Iteration (do)
      
      (exports (scheme vector))
      (exports (scheme bytevector))

      (exports (scheme misc)) ; string->number
      (exports (scheme exceptions))
      (exports (scheme read))

      (exports (owl list))
      (exports (owl rlist))
      (exports (owl list-extra))
      (exports (owl ff))
      (exports (owl io))
      (exports (owl lazy))
      (exports (owl string))
      (exports (owl sort))
      (exports (otus blobs))
      (exports (otus format))
      (exports (otus async))
      (exports (otus fasl))
      (exports (owl time))
      (exports (owl regex))
      (exports (owl math-extra))
      (exports (owl math))

      (exports (lib system))

      ; extended ol functions
      ; universal for lists, vectors and strings
      for-each fold
      =
      
      (exports (lang error)))

   (import
      (scheme core)
      (srfi 1)
      (srfi 87)
      (scheme base)

      (owl list)
      (owl rlist)
      (owl list-extra)
      (owl ff)
      (owl io)
      (owl time)
      (owl lazy)
      (owl math-extra)
      (owl string)
      (owl sort)
      (otus fasl)
      (otus blobs)
      (owl regex)
      (otus format)
      (otus async)
      (owl math)

      (lib system)

      (scheme vector)
      (scheme bytevector)
      (scheme misc)
      (scheme exceptions)
      (scheme read)

      (lang error))

(begin
   
   ; internal function
   (define (->list x)
      (cond
         ((string? x)
            (string->list x))
         ((vector? x)
            (vector->list x))
         ((bytevector? x)
            (bytevector->list x))
         (else
            x)))

   ; * experimental features

   ; ol `for-each` and `fold` accepts vectors, lists and strings
   ;  as function arguments in one manner
   (define for-each (case-lambda
      ((f a)      (for-each f (->list a)))
      ((f a b)    (for-each f (->list a)
                              (->list b)))
      ((f a b . c)
                  (let loop ((a (map ->list (cons* a b c))))
                     (unless (null? (car a))
                        (apply f (map car a))
                        (loop (map cdr a)))))
      ((f) #false)))

   (define fold (case-lambda
      ((f state a)      (fold f state (->list a)))
      ((f state a b)    (fold f state (->list a)
                                      (->list b)))
      ((f state a b . c)
                        (let loop ((state state) (args (map ->list (cons* a b c))))
                           (if (null? (car args))
                              state
                              (loop (apply f (cons state (map car args))) (map cdr args)))))
      ((f state) state)))

   ; -=( universal = )=-------------------
   (define =
      (case-lambda
         ((a b)
            (cond
               ((number? a)
                  (number=? a b))
               ((string? a)
                  (string=? a b))
               (else
                  (equal? a b))))

         ((a . bs)
            (cond
               ((number? a)
                  (each (lambda (b)
                           (number=? a b))
                     bs))
               ((string? a)
                  (each (lambda (b)
                           (string=? a b))
                     bs))
               (else
                  (each (lambda (b)
                           (equal? a b))
                     bs)) ))))

))
