(define-library (scheme read)
(export
   read
   readline)

(import
   (scheme core)
   (only (lang sexp) sexp)
   (owl io)
   (owl io scheduler)
   (owl parse)
   (owl string)
   (otus async))

(begin

   (define (unbuffered-input-stream port)
      (lambda ()
         (define in (syscall 0 port 1))
         (case in
            (#f #null)  ; port error
            (#t ; input is not ready
               (if (eq? port stdin)
                  (sleep 2)
                  (wait-read port 3000)) ; 3 seconds wait
               (unbuffered-input-stream port))
            (#eof      ; end-of-file
               (unless (eq? port stdin)
                  (close-port port))
               #null)
            (else
               (cons (ref in 0) (unbuffered-input-stream port))))))

   ; * internal function
   (define (sexp-reader port)
      (let* ((l r p val ((sexp)
                           #null ; no left part of stream
                           (unbuffered-input-stream port)
                           0 ; start position in the stream
                           (λ (l r p v) ; ok
                              (values l r p v)))))
         (when l
            val)))

   (define read (case-lambda
      ((port)
         (sexp-reader port))
      (()
         (sexp-reader stdin))))


   (define (line-reader port)
      (let* ((l r p val ((let-parse* (
                              (runes (greedy* (rune-if (lambda (r) (not (eq? r #\newline))))))
                              (newline (either (imm #\newline) (epsilon #eof))))
                           (if (and (null? runes) (eq? newline #eof))
                              #false
                              (runes->string runes)))
                           #null ; no left part of stream
                           (unbuffered-input-stream port)
                           0 ; start position in the stream
                           (λ (l r p v) ; ok
                              (values l r p v)))))
         (when l
            val)))

   (define readline (case-lambda
      ((port)
         (line-reader port))
      (()
         (line-reader stdin))))

))
