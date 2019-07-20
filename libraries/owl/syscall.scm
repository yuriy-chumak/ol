(define-library (owl syscall)
   (export 
      gettimeofday
      strftime)

   (import
      (scheme core))

   (begin

      (define (gettimeofday) (syscall2 96))
      (define (strftime fmt) (syscall2 201 fmt))
))