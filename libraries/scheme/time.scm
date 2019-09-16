(define-library (scheme time)
   (export 
      current-jiffy
      current-second
      jiffies-per-second
      
   )

   (import
      (scheme core))

   (begin

      (define (current-jiffy)
         (cdr (syscall 96)))

      (define (current-second)
         (car (syscall 96)))

      (define (jiffies-per-second)
         1000000)
      
))
