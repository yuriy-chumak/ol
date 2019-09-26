(define-library (scheme file)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme file))
   (description "
      Scheme file library.")

   (import
      (scheme core))

   (export 
      file-exists?
      delete-file)

   (begin

      (define (file-exists? filename)
         (if (syscall 4 (c-string filename))
            #true))

      (define (delete-file filename)
         (syscall 87 (c-string filename)))

))
