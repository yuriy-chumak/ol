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
      ;; call-with-output-file
      ;; open-binary-input-file
      ;; open-binary-output-file
      ;; open-input-file
      ;; open-output-file
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
         (let ((port (open-input-file filename)))
            (if port
               (let ((result (proc port)))
                  (close-port port)
                  result))))

))
