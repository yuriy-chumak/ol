(define-library (otus syscall) ; todo: rename to (sys calls) ?
   (export 
      gettimeofday
      strftime)

   (import
      (scheme core)
      (owl string))

   (begin

      (define (gettimeofday) (syscall 96))
      (define strftime (case-lambda
         ((fmt) (syscall 201 (c-string fmt)))
         ((fmt time) (syscall 201 (c-string fmt) time))))
))