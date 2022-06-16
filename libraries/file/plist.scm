(define-library (file plist)
   (export
      plist-parser
      
      read-plist
      read-plist-file

      read-plist-port   ; same as read-plist
      read-plist-string ; same as read-plist
      read-plist-stream ; same as read-plist
   )
   (import
      (otus lisp)
      (owl parse)
      (lang sexp))
(begin

   (define whitespaces {
      #\tab #T
      #\newline #T
      #\space #T
      #\return #T
   })
   (define whitespace (byte-if (lambda (x) (whitespaces x #false))))

   (define (block-comment)
      (either
         (let-parse*(
               (skip (imm #\*))
               (skip (imm #\/)))
            'comment)
         (let-parse*(
               (skip byte)
               (skip (block-comment)))
            skip)))

   (define whitespace-or-comment
      (any-of
         whitespace
         (let-parse*(
               (skip (imm #\/))
               (skip (imm #\/))
               (skip rest-of-line))
            'comment)
         (let-parse*(
               (skip (imm #\/))
               (skip (imm #\*))
               (skip (block-comment)))
            'comment)))

   (define maybe-whitespaces (greedy* whitespace-or-comment))

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

   (define ansi-chars (fold (lambda (ff char) (put ff char #T))
         #empty
         (string->runes "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz_")))

   (define string-char
      (let-parse* (
            (char (byte-if (lambda (byte) (ansi-chars byte #false)))))
         char))

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
      (any-of
         ; just_a_string
         (let-parse* (
               (chars (greedy+ string-char)))
            (runes->string chars))
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
               ;; (word "true" #true)
               ;; (word "false" #false)
               ;; (word "null" #null)
               number
               ; objects:
               (let-parse* (
                     (/ (imm #\{))
                     (kv (greedy*
                        (let-parse* (
                              (/ maybe-whitespaces)
                              (key string)

                              (/ maybe-whitespaces)
                              (/ (imm #\=))
                              (/ maybe-whitespaces)

                              (value (object))

                              (/ maybe-whitespaces)
                              (/ (any-of
                                    (imm #\,)
                                    (imm #\;)
                                    (epsilon #f))))
                           (cons key value))))
                     (/ maybe-whitespaces)
                     (/ (imm #\})))
                  (fold (lambda (ff kv)
                           (put ff (string->symbol (car kv)) (cdr kv)))
                     #empty kv))
               ; vectors
               (let-parse* (
                     (/ (imm #\())
                     (value (greedy*
                        (let-parse* (
                              (value (object))
                              (/ maybe-whitespaces)
                              (/ (any-of
                                    (imm #\,)
                                    (imm #\;)
                                    (epsilon #f))))
                           value)))
                     (/ maybe-whitespaces)
                     (/ (imm #\))))
                  (list->vector value)))))
         value))

   (define plist-parser (object))


   (define (read-plist-stream stream)
      (when stream
         (define plist (try-parse plist-parser stream #f))
         (if plist (car plist))))

   (define (read-plist-port port)
      (when port
         (read-plist-stream (force (port->bytestream port)))))

   (define (read-plist-string str)
      (when str
         (read-plist-stream (str-iter str))))

   (define read-plist (case-lambda
      (() (read-plist-port stdin))
      ((source) (cond
         ((port? source) (read-plist-port source))
         ((string? source) (read-plist-string source))
         ((pair? source) (read-plist-stream source))))))

   (define (read-plist-file filename)
      (read-plist (if (equal? filename "-")
                     stdin
                     (open-input-file filename)))) ; note: no need to close port

))
