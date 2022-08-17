(define-library (file sexp)
   (export
      sexp-parser
      
      read-sexp
      read-sexp-file

      read-sexp-port
      read-sexp-string
      read-sexp-stream

      ;; write-sexp
      ;; write-sexp-file)
   )
   (import
      (otus lisp)
      (owl parse) (owl io)
      (owl unicode)
      (lang sexp))
(begin

   (define (print-sexp-to port object)
      (define (display x) (display-to port x))
      (let sexpify ((L object))
         (cond
            ((string? L)
               (write-bytes port (cons #\" (str-foldr (lambda (ch tl)
                     (case ch
                        (#\" (cons* #\\ #\" tl))
                        (else
                           (encode-point ch tl))))
                  '(#\") L))))
            ((symbol? L)
               (write L))
            ((inexact? L)
               (write-bytes port '(#\# #\i))
               (display L))
            ((vector? L)
               (display "[")
               (let ((len (size L)))
                  (let loop ((n 1))
                     (unless (less? len n)
                        (sexpify (ref L n))
                        (if (less? n len)
                           (display " "))
                        (loop (+ n 1)))))
               (display "]"))
            ((list? L)
               (display "(list")
                  (let loop ((L L))
                     (unless (null? L)
                        (display " ")
                        (sexpify (car L))
                        (loop (cdr L))))
               (display ")"))
            ((ff? L)
               (display "{")
               (let loop ((L (ff-iter L)))
                  (cond
                     ((pair? L)
                        (sexpify (caar L))
                        (display " ")
                        (sexpify (cdar L))
                        (display " ")
                        (loop (cdr L)))
                     ((function? L)
                        (loop (L)))))
               (display "}"))
            (else
               (display L)))))

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

   ;; (setq write-to (writer-to {}))

   (define (write-sexp sexp port)
      (print-sexp-to port sexp))
   (define write-sexp (case-lambda
      ((sexp) (write-sexp sexp stdout))
      ((sexp port) (write-sexp sexp port))))

   (define (write-sexp-file sexp filename)
      (define port (if (equal? filename "-")
                     stdout
                     (open-output-file filename)))
      (write-sexp sexp port)
      (unless (eq? port stdout)
         (close-port port)))

))
