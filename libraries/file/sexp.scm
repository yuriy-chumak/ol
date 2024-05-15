(define-library (file sexp)
   (export
      sexp-parser
      
      read-sexp
      read-sexp-file

      read-sexp-port
      read-sexp-string
      read-sexp-stream

      write-sexp
      write-sexp-file)

   (import
      (otus lisp)
      (owl parse) (owl io)
      (owl unicode)
      (lang sexp))
(begin

   (define sexp-parser sexp-parser)

   (define (read-sexp-stream stream)
      (when stream
         (define sexp (try-parse sexp-parser stream #f))
         (if sexp (car sexp))))

   (define (read-sexp-port port)
      (when port
         (read-sexp-stream (force (port->bytestream port)))))

   (define (read-sexp-string str)
      (when str
         (read-sexp-stream (str-iter str))))

   (define read-sexp (case-lambda
      (() (read-sexp-port stdin))
      ((source) (cond
         ((port? source) (read-sexp-port source))
         ((string? source) (read-sexp-string source))
         ((pair? source) (read-sexp-stream source))))))

   (define (read-sexp-file filename)
      (read-sexp (if (equal? filename "-")
                     stdin
                     (open-input-file filename)))) ; note: no need to close port

   (define write-sexp write)

   (define (write-sexp-file sexp filename)
      (define port (if (equal? filename "-")
                     stdout
                     (open-output-file filename)))
      (write-sexp sexp port)
      (unless (eq? port stdout)
         (close-port port)))

))
