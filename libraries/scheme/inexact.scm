(define-library (scheme inexact)
   (export 
      finite? infinite? nan?

      exp log
      sin cos tan
      asin acos atan

      sqrt)

   (import
      (scheme core)
      (owl math)
      (owl math fp)
      (owl math-extra)
      (owl io))
(begin
   ;; (when (eq? (vm:and (vm:features) #o40) 0)
   ;;    (print-to stderr "Warning: OL built without OLVM_BUILTIN_FMATH support. SQRT and other math functions will return #false."))

   (define (nan-or-inf? z)
      (or (equal? z +nan.0)
          (equal? z +inf.0)
          (equal? z -inf.0)))


   (define (nan? z)
      (or
         (equal? z +nan.0)
         (when (eq? (type z) type-complex)
            (or (equal? (car z) +nan.0)
                (equal? (cdr z) +nan.0)))))

   (assert (nan? +nan.0)      ===> #true)
   (assert (nan? 32)          ===> #false)
   (assert (nan? +nan.0+5.0i) ===> #true)
   (assert (nan? 5.0++nan.0i) ===> #true)
   (assert (nan? 1+2i)        ===> #false)

   (define (infinite? z)
      (or
         (nan-or-inf? z)
         (when (eq? (type z) type-complex)
            (or (nan-or-inf? (car z))
                (nan-or-inf? (cdr z))))))

   (define (finite? z)
      (when (number? z)
         (not (infinite? z))))


   ;; sqrt n → m such that m^2 = n
   ; как посчитать корень квадратный:
   ; 1. попытаться найти целочисленный результат,
   ; 2. если не вышло, то используя итерационный алгоритм Ньютона с указанной точностью попытаться найти более удовлетворительный

   (define (good-enough? guess1 guess0 precision)
      (< (abs (- guess1 guess0)) (abs (* guess0 precision))))
   (define (better-guess guess x) (/ (+ guess (/ x guess)) 2))

   (define (:sqrt n precision)
      (case (type n)
         (type-enum+
            (let*((s r (exact-integer-sqrt n))) ; r: remainder
               (cond
                  ((eq? r 0)
                     s)
                  ((eq? precision 0)
                     (runtime-error "sqrt: no exact solution for " n))
                  (else
                     (let loop ((s s))
                        (let ((t (better-guess s n)))
                           ;(print "s: " s ", t: t")
                           (if (good-enough? t s precision)
                              t
                              (loop t))))))))
         (type-int+
            (let*((s r (exact-integer-sqrt n))) ; r: remainder
               (cond
                  ((eq? r 0)
                     s)
                  ((eq? precision 0)
                     (runtime-error "sqrt: no exact solution for " n))
                  (else
                     (let loop ((s s))
                        (let ((t (better-guess s n)))
                           ;(print "s: " s ", t: t")
                           (if (good-enough? t s precision)
                              t
                              (loop t))))))))
         (type-enum-
            (complex 0 (:sqrt (abs n) precision)))
         (type-int-
            (complex 0 (:sqrt (abs n) precision)))
         (type-inexact
            (if (fless? n 0)
               (complex 0 (fsqrt (fsub 0 n)))
               (fsqrt n)))
         (type-rational
            (if (< n 0)
               (complex 0 (fsqrt (fsub 0 n)))
               (fsqrt n)))
         (else
            (runtime-error "sqrt: math not applicable: " n))))

   (define sqrt
      (case-lambda
         ((n accuracy)
            (:sqrt n accuracy))
         ((n)
            (:sqrt n 0.0001)))) ; 1/10000

;   (assert (sqrt 9)  ===> #i3.0)

   (define exp fexp)
   (define log
      (case-lambda
         ((n)
            (flog n))
         ((n base)
            (flog n base))))

   (define sin fsin)
   (define cos fcos)
   (define tan ftan)

   (define asin fasin)
   (define acos facos)
   (define atan (case-lambda
      ((n)
         (fatan n))
      ((y x)
         (fatan2 y x))))

))
