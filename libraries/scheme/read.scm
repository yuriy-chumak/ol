(define-library (scheme read)
(export
   read)

(import
   (scheme core)
   (owl io)
   (only (lang sexp) sexp)
   (otus async))

(begin

   (define (unbuffered-input-stream port)
      (lambda ()
         (define in (syscall 0 port 1))
         (case in
            (#f #null)  ; port error
            (#t ; input is not ready
               (await (mail io-scheduler-name ['read-timeout port 3000])) ; 3 second wait
               (unbuffered-input-stream port))
            (#eof      ; end-of-file
               (unless (eq? port stdin)
                  (close-port port))
               #null)
            (else
               (cons (ref in 0) (unbuffered-input-stream port))))))

   ; * internal function
   (define (read-impl port)
      (let* ((l r p val ((sexp)
                           #null ; no left part of stream
                           (unbuffered-input-stream port)
                           0 ; start position in the stream
                           (λ (l r p v) ; ok
                              (values l r p v)))))
         (when l
            val)))

   ; public
   (define read (case-lambda
      ((port)
         (read-impl port))
      (()
         (read-impl stdin))))

))
