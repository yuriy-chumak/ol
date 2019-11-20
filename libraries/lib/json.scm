(define-library (lib json)
   (export
      print-json-with
      json-parser)
   (import
      (otus lisp)
      (lang intern)
      (lang sexp)
      (owl parse))

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
            (let*((int (floor L))
                  (frac (floor (* (- L int) 10000))))
            (display int) (display ".")
            (let loop ((i frac) (n 1000))
               (display (floor (/ i n)))
               (if (less? 1 n)
                  (loop (mod i n) (/ n 10))))))
         ;; ((blob? L)
         ;;    (display "[")
         ;;    (let ((len (blob-len L)))
         ;;       (let loop ((n 0))
         ;;          (if (less? n len) (begin
         ;;             (jsonify (blob-ref L n))
         ;;             (if (less? (+ n 1) len)
         ;;                (display ","))
         ;;             (loop (+ n 1))))))
         ;;    (display "]"))
         ((vector? L)
            (display "[")
            (let ((len (size L)))
               (let loop ((n 1))
                  (unless (less? len n) (begin
                     (jsonify (ref L n))
                     (if (less? n len)
                        (display ","))
                     (loop (+ n 1))))))
            (display "]"))
         ;; ((list? L)
         ;;    (display "{")
         ;;    (let loop ((L L))
         ;;       (unless (null? L) (begin
         ;;          (for-each display `("\"" ,(caar L) "\":"))
         ;;          (jsonify (cdar L))
         ;;          (if (not (null? (cdr L)))
         ;;             (display ","))
         ;;          (loop (cdr L)))))
         ;;    (display "}"))
         ((ff? L)
            (display "{")
            (let loop ((L (ff-iter L)) (comma #f))
               (cond
                  ((pair? L)
                     (if comma (display ","))
                     (for-each display `("'" ,(caar L) "':"))
                     (jsonify (cdar L))
                     (loop (cdr L) #t))
                  ((function? L)
                     (loop (L) #t))))
            (display "}"))
         )))

(define get-a-whitespace (get-byte-if (lambda (x) (has? '(#\tab #\newline #\space #\return) x))))
(define maybe-whitespaces (get-kleene* get-a-whitespace))

(define get-string (get-either
   (let-parses (
         (begin (get-imm #\'))
         (runes (get-kleene* (get-rune-if (lambda (rune) (not (eq? rune #\'))))))
         (end (get-imm #\')))
      (runes->string runes))
   (let-parses (
         (begin (get-imm #\")) ;"
         (runes (get-kleene* (get-rune-if (lambda (rune) (not (eq? rune #\")))))) ;"
         (end (get-imm #\"))) ;"
      (runes->string runes))))

(define get-number
   (let-parses (
         (value (get-kleene+ (get-rune-if (lambda (rune) (<= #\0 rune #\9))))))
      (list->number value 10)))

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

; few parsing examples:
   (assert (car (try-parse json-parser (str-iter "{}") #t))
      ===> #empty)
   (assert (car (try-parse json-parser (str-iter "[]") #t))
      ===> [])
   (assert (car (try-parse json-parser (str-iter "{'something':[12,23,34],'new':true}") #t))
      ===> (pairs->ff `((something . ,[12 23 34]) (new . #true))))

))
