; otus lisp language
(define-library (otus lisp)

   (export
      (exports (scheme base))
      
      ; all Ol functions, not only (scheme base) profile:
      (exports (scheme srfi-1))  ; * List Library
;      (exports (scheme r5rs iteration)) ; 4.2.4 Iteration (do)
      
      (exports (scheme vector))
      (exports (scheme bytevector))

      ;(exports (scheme read))
      (exports (scheme misc)) ; string->number
      (exports (scheme exceptions))

      (exports (owl list))
      (exports (owl rlist))
      (exports (owl list-extra))
      (exports (owl ff))
      (exports (owl io))
      (exports (owl lazy))
      (exports (owl string))
      (exports (owl sort))
      (exports (otus blobs))
      (exports (owl render))
      (exports (otus async))
      (exports (otus fasl))
      (exports (owl time))
      (exports (owl regex))
      (exports (owl math-extra))
      (exports (owl math))

      ; extended ol functions
      for-each ; universal for lists, vectors and strings
      
      )

   (import
      (scheme core)
      (scheme srfi-1)
      (scheme srfi-87)
      (scheme base)
      ;(scheme read)

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
      (owl render)
      (otus async)
      (owl math)

      (scheme vector)
      (scheme bytevector)
      (scheme misc)
      (scheme exceptions)
   )

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

   ; ol for-each accepts vectors, lists and strings as function arguments in one manner
   ; * experimental feature
   (define for-each (case-lambda
      ((f a)      (let loop ((a (->list a)))
                     (unless (null? a)
                        (f (car a))
                        (loop (cdr a)))))
      ((f a b)    (let loop ((a (->list a))
                             (b (->list b)))
                     (unless (null? a)
                        (f (car a) (car b))
                        (loop (cdr a) (cdr b)))))
      ((f a b . c)
                  (let loop ((a (map ->list (cons a (cons b c)))))
                     (unless (null? (car a))
                        (apply f (map car a))
                        (loop (map cdr a)))))
      ((f) #false)))
))