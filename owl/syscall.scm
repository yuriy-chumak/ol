(define-library (owl syscall)
   (export 
      gettimeofday
      strftime)

   (import
      (r5rs core))

   (begin

      (define (gettimeofday) (syscall 96 #f #f #f))
      (define (strftime fmt) (syscall 201 fmt #f #f))
))