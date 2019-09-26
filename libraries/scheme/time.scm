; 
(define-library (scheme time)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme time))
   (description "
      Scheme time library.")

   (import
      (scheme core))

   (export 
      current-jiffy
      current-second
      jiffies-per-second
      
   )

   (begin

      (define (current-jiffy)
         (cdr (syscall 96)))

      (define (current-second)
         (car (syscall 96)))

      (define (jiffies-per-second)
         1000000)
      
))
