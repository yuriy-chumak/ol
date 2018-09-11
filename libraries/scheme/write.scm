(define-library (scheme write)
   (export
      display
      write
      write-shared
      write-simple)

   (import
      (scheme core)
      (only (owl io) display write))

   (begin

(define (write-shared . args)
   (runtime-error "Not implemented: " 'write-shared))
(define (write-simple . args)
   (runtime-error "Not implemented: " 'write-simple))

))
