(define-library (otus syscall) ; todo: rename to (sys calls) ?
   (export 
      gettimeofday
      strftime)

   (import
      (scheme core))

   (begin

      (define (gettimeofday) (syscall 96))
      (define (strftime fmt) (syscall 201 fmt))
))