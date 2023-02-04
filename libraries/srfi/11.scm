(define-library (srfi 11)
; http://srfi.schemers.org/srfi-11/srfi-11.html

(export
   let-values let*-values
   let-vector let*-vector)

(import
   (scheme core))

(begin

   (define-syntax let-values
      (syntax-rules ()
         ((let-values ((var ...) val) . body)
            (values-apply val (lambda (var ...) . body)))))

   (define-syntax let*-values
      (syntax-rules ()
         ((let*-values (((var ...) gen) .rest) . body)
            (values-apply gen (lambda (var ...)
               (let*-values rest . body))))
         ((let*-values ((var ...) val) . body)
            (values-apply val (lambda (var ...) . body)))

         ((let*-values () exp . rest)
            (begin exp . rest))))

   (define-syntax let-vector
      (syntax-rules ()
         ((let-values ((var ...) val) . body)
            (vector-apply val (lambda (var ...) . body)))))

   (define-syntax let*-vector
      (syntax-rules ()
         ((let*-vector (((var ...) gen) .rest) . body)
            (vector-apply gen (lambda (var ...)
               (let*-vector rest . body))))
         ((let*-vector ((var ...) val) . body)
            (vector-apply val (lambda (var ...) . body)))

         ((let*-vector () exp . rest)
            (begin exp . rest))))

))
