(define-library (otus syscall)
   (export 
      gettimeofday
      strftime)

   (import
      (scheme core))

   (begin

      (define (gettimeofday) (syscall 96))
      (define (strftime fmt) (syscall 201 fmt))
))