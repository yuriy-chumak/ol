(define-library (scheme complex)
   (export 
      ; make-rectangular make-polar
      real-part imag-part)

   (import
      (scheme core))

(begin
   (define real-part car)
   (define imag-part cdr)
))
