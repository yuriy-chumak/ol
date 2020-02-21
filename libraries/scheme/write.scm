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

   ; note: we do not support shared labels for now
   (define write-shared write)

   (define write-simple write)

))
