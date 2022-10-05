(define-library (file json)
   ; todo: add function toi encode json into stream
   (export
      print-json-with
      json-parser
      
      read-json
      read-json-file

      read-json-port   ; same as read-json
      read-json-string ; same as read-json
      read-json-stream ; same as read-json

      write-json
      write-json-file)
   (import
      (otus lisp)
      (owl parse)
      (lang sexp))
(begin

   (define (print-json-with display object)
      (let jsonify ((L object))
         (cond
            ((string? L)
               (for-each display `("\"" ,L "\"")))
            ((boolean? L)
               (display (if L "true" "false")))

            ((integer? L)
               (display L))
            ((rational? L)
               (let*((sign (< L 0))
                     (L (abs L))
                     (int (floor L))
                     (frac (floor (* (- L int) 10000))))
               (if sign (display "-"))
               (display int) (display ".")
               (let loop ((i frac) (n 1000))
                  (display (floor (/ i n)))
                  (if (less? 1 n)
                     (loop (mod i n) (/ n 10))))))
            ((inexact? L)
               (display L))
            ((vector? L)
               (display "[")
               (let ((len (size L)))
                  (let loop ((n 1))
                     (unless (less? len n)
                        (jsonify (ref L n))
                        (if (less? n len)
                           (display ","))
                        (loop (+ n 1)))))
               (display "]"))
            ((ff? L)
               (display "{")
               (let loop ((L (ff-iter L)) (comma #f))
                  (cond
                     ((pair? L)
                        (if comma (display ","))
                        (for-each display `("\"" ,(symbol->string (caar L)) "\":"))
                        (jsonify (cdar L))
                        (loop (cdr L) #t))
                     ((function? L)
                        (loop (L) #t))))
               (display "}"))
            )))

   (define whitespace (byte-if (lambda (x) (has? '(#\tab #\newline #\space #\return) x))))
   (define maybe-whitespaces (greedy* whitespace))

   ; https://www.ietf.org/rfc/rfc4627.txt
   (define quoted-values {
      #\"  #\" ;22
      #\\  #\\ ;5c
      #\/  #\/ ;2f
      #\b  #x8
      #\f  #xc
      #\n  #xa
      #\r  #xd
      #\t  #x9
   })

   (define get-quoted-string-char
      (let-parse* (
            (skip (imm #\\))
            (char (either
                     (let-parse* (
                           (char (byte-if (lambda (byte) (quoted-values byte)))))
                        (quoted-values char))
                     (let-parse* (
                           (skip (imm #\u))
                           (h1 get-rune)
                           (h2 get-rune)
                           (h3 get-rune)
                           (h4 get-rune))
                        (list->number (list h1 h2 h3 h4) 16)))))
         char))

   (define string
      (either
         (let-parse* (
               (skip (imm #\"))
               (chars (greedy*
                        (either
                           get-quoted-string-char
                           (rune-if (lambda (x) (not (has? '(#\" #\\) x)))))))
               (skip (imm #\")))
            (runes->string chars))
         (let-parse* (
               (skip (imm #\'))
               (chars (greedy*
                        (either
                           get-quoted-string-char
                           (rune-if (lambda (x) (not (has? '(#\' #\\) x)))))))
               (skip (imm #\')))
            (runes->string chars))))

   (define natural
      (let-parse* (
            (value (greedy+ (rune-if (lambda (rune) (<= #\0 rune #\9))))))
         (list->number value 10)))

   (define number
      (let-parse* (
            (signer (any-of
                  (word "+" (lambda (x) x))
                  (word "-" (lambda (x) (negate x)))
                  (epsilon  (lambda (x) x))))
            (int natural)
            (frac (either
                        (let-parse* (
                              (skip (imm #\.))
                              (digits (greedy* (rune-if (lambda (rune) (<= #\0 rune #\9))))))
                           (/ (list->number digits 10) (expt 10 (length digits))))
                        (epsilon 0)))
            (exponent (any-of
                  (let-parse* (
                        (e (imm #\e))
                        (signer (any-of
                           (word "+" (lambda (x) x))
                           (word "-" (lambda (x) (negate x)))
                           (epsilon  (lambda (x) x))))
                        (exp (greedy+ (rune-if (lambda (rune) (<= #\0 rune #\9))))))
                     (expt 10 (signer (list->number exp 10))))
                  (epsilon 1))))
         (* (signer (+ int frac)) exponent)))

   (define (object)
      (let-parse* (
            (/ maybe-whitespaces) ; skip leading whitespaces
            (value (any-of
               string
               (word "true" #true)
               (word "false" #false)
               (word "null" #null)
               number
               ; objects:
               (let-parse* (
                     (/ (imm #\{))
                     (kv (greedy*
                        (let-parse* (
                              (/ maybe-whitespaces)
                              (key string)
                              (/ maybe-whitespaces)
                              (/ (imm #\:))
                              (value (object))
                              (/ maybe-whitespaces)
                              (/ (greedy* (byte-if (lambda (x) (eq? x #\,))))))
                           (cons key value))))
                     (/ maybe-whitespaces)
                     (/ (imm #\})))
                  (fold (lambda (ff kv)
                           (put ff (string->symbol (car kv)) (cdr kv)))
                     #empty kv))
               ; vectors
               (let-parse* (
                     (/ (imm #\[))
                     (value (greedy*
                        (let-parse* (
                              (value (object))
                              (/ maybe-whitespaces)
                              (/ (greedy* (byte-if (lambda (x) (eq? x #\,))))))
                           value)))
                     (/ maybe-whitespaces)
                     (/ (imm #\])))
                  (list->vector value)))))
         value))

   (define json-parser (object))


   (define (read-json-stream stream)
      (when stream
         (define json (try-parse json-parser stream #f))
         (if json (car json))))

   (define (read-json-port port)
      (when port
         (read-json-stream (force (port->bytestream port)))))

   (define (read-json-string str)
      (when str
         (read-json-stream (str-iter-bytes str))))

   (define read-json (case-lambda
      (() (read-json-port stdin))
      ((source) (cond
         ((port? source) (read-json-port source))
         ((string? source) (read-json-string source))
         ((pair? source) (read-json-stream source))))))

   (define (read-json-file filename)
      (read-json (if (equal? filename "-")
                     stdin
                     (open-input-file filename)))) ; note: no need to close port


   (define (write-json json port)
      (print-json-with (lambda (what) (display-to port what)) json))
   (define write-json (case-lambda
      ((json) (write-json json stdout))
      ((json port) (write-json json port))))

   (define (write-json-file json filename)
      (define port (if (equal? filename "-")
                     stdout
                     (open-output-file filename)))
      (write-json json port)
      (unless (eq? port stdout)
         (close-port port)))

))
