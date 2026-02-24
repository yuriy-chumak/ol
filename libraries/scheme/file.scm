(define-library (scheme file)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme file))
   (description "
      Scheme file library.")

   (import
      (scheme core)
      (owl string) (owl io))

   (export
      call-with-input-file
      call-with-output-file
      open-binary-input-file
      open-binary-output-file
      open-input-file
      open-output-file
      ;; with-input-from-file
      ;; with-output-to-file

      file-exists?
      delete-file)


   (begin

      (define (file-exists? filename)
         (if (syscall 4 (c-string filename))
            #true))

      (define (delete-file filename)
         (syscall 87 (c-string filename)))

      (define (call-with-input-file filename proc)
         (call-with-port (open-input-file filename) proc))

      (define (call-with-output-file filename proc)
         (call-with-port (open-output-file filename) proc))

      (define (sys:open path mode)
         (cond
            ((c-string path) =>
               (λ (path) (syscall 2 path mode)))))

      (define (open-binary-input-file path)
         (sys:open path #o100000)) ; O_RDONLY|O_BINARY
      (define (open-binary-output-file path)
         (sys:open path #o101102)) ; O_CREAT|O_TRUNC|O_RDWR+O_BINARY

      (define (open-input-file path)
         (sys:open path #o0000))  ; O_RDONLY
      (define (open-output-file path)
         (sys:open path #o1102)) ; O_CREAT|O_TRUNC|O_RDWR

))
