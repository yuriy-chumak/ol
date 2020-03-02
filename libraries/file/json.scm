(define-library (file json)
   (export
      print-json-with
      json-parser
      
      read-json
      read-json-file
      read-json-string
      write-json
      write-json-file)
   (import
      (otus lisp)
      (lang sexp)
      (owl parse)
      (file parser))
(begin

   (define (print-json-with display object)
      (let jsonify ((L object))
         (cond
            ((symbol? L)
               (for-each display `("\"" ,L "\"")))
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
                        (for-each display `("\"" ,(caar L) "\":"))
                        (jsonify (cdar L))
                        (loop (cdr L) #t))
                     ((function? L)
                        (loop (L) #t))))
               (display "}"))
            )))

   (define get-a-whitespace (get-byte-if (lambda (x) (has? '(#\tab #\newline #\space #\return) x))))
   (define maybe-whitespaces (get-kleene* get-a-whitespace))

   ; https://www.ietf.org/rfc/rfc4627.txt
   (define quoted-values {
      #\"  #\" ;22
      #\\  #\\ ;5c
      #\/  #\/ ;2f
      #\b  #x0008
      #\f  #x000c
      #\n  #x000a
      #\r  #x000d
      #\t  #x0009
   })

   (define get-quoted-string-char
      (let-parses (
            (skip (get-imm #\\))
            (char (get-either
                     (let-parses (
                           (char (get-byte-if (lambda (byte) (getf quoted-values byte)))))
                        (getf quoted-values char))
                     (let-parses (
                           (skip (get-imm #\u))
                           (hexes (get-n-times 4 get-rune)))
                        (list->number hexes 16)))))
         char))

   (define get-string (get-either
      (let-parses (
            (skip (get-imm #\"))
            (chars (get-kleene*
                     (get-either
                        get-quoted-string-char
                        (get-rune-if (lambda (x) (not (has? (list #\" #\\) x)))))))
            (skip (get-imm #\")))
         (runes->string chars))
      (let-parses (
            (skip (get-imm #\'))
            (chars (get-kleene*
                     (get-either
                        get-quoted-string-char
                        (get-rune-if (lambda (x) (not (has? (list #\' #\\) x)))))))
            (skip (get-imm #\')))
         (runes->string chars))))

   (define get-natural
      (let-parses (
            (value (get-kleene+ (get-rune-if (lambda (rune) (<= #\0 rune #\9))))))
         (list->number value 10)))

   (define get-number
      (let-parses (
            (signer (get-any-of
                  (get-word "+" (lambda (x) x))
                  (get-word "-" (lambda (x) (- x)))
                  (get-epsilon  (lambda (x) x))))
            (int get-natural)
            (frac (get-either
                        (let-parses (
                              (skip (get-imm #\.))
                              (digits (get-greedy* (get-rune-if (lambda (rune) (<= #\0 rune #\9))))))
                           (/ (list->number digits 10) (expt 10 (length digits))))
                        (get-epsilon 0))))
                  ;(pow get-exponent))
               ;(sign (* (+ num tail) pow))))
         (signer (+ int frac))))

   (define (get-object)
      (let-parses (
            (/ maybe-whitespaces)
            (value (get-any-of
               get-string
               (get-word "true" #true)
               (get-word "false" #false)
               get-number
               ; objects:
               (let-parses (
                     (/ (get-imm #\{))
                     (kv (get-kleene*
                        (let-parses (
                              (/ maybe-whitespaces)
                              (key get-string)
                              (/ maybe-whitespaces)
                              (/ (get-imm #\:))
                              (value (get-object))
                              (/ maybe-whitespaces)
                              (/ (get-kleene* (get-byte-if (lambda (x) (eq? x #\,))))))
                           (cons key value))))
                     (/ maybe-whitespaces)
                     (/ (get-imm #\})))
                  (fold (lambda (ff kv)
                           (put ff (string->symbol (car kv)) (cdr kv)))
                     #empty kv))
               ; vectors
               (let-parses (
                     (/ (get-imm #\[))
                     (value (get-kleene*
                        (let-parses (
                              (value (get-object))
                              (/ maybe-whitespaces)
                              (/ (get-kleene* (get-byte-if (lambda (x) (eq? x #\,))))))
                           value)))
                     (/ maybe-whitespaces)
                     (/ (get-imm #\])))
                  (list->vector value)))))
         value))

   (define json-parser (get-object))

   ; sanity check:
   (assert (car (try-parse json-parser (str-iter "{}") #t))
      ===> #empty)
   (assert (car (try-parse json-parser (str-iter "[]") #t))
      ===> [])
   (assert (car (try-parse json-parser (str-iter "{'something':[12,23,34],'old':{},'new':true}") #t))
      ===> { 'something [12 23 34]
               'old #empty
               'new #true })

   (define (read-json port)
      (define json (try-parse json-parser (force (port->bytestream port)) #f))
      (if json (car json)))

   (define read-json (case-lambda
      (() (read-json stdin))
      ((port) (read-json port))))

   (define (read-json-file filename)
      (read-json (if (equal? filename "-")
                     stdin
                     (open-input-file filename)))) ; note: no need to close port

   (define (read-json-string str)
      (define json (try-parse json-parser (str-iter str) #f))
      (if json (car json)))

   (define (write-json json port)
      (print-json-with (lambda (what) (display-to port)) json))
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
