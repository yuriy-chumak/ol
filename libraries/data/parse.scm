(define-library (data parse)
   (export
      parse     ; (parse parser data fail-val) → result | fail-val (if no full match)
                ; если же надо проверить, что больше ничего не осталось - используем специальный парсер "eof"
      let-parse ; returns pair '(value . stream), stream is #f if error, stream is #n if eof

      let-parse*; parser declarator
      backtrack ; rollback stream if fail (internal, but shared for other parsers)

      ; basic parsers:
      byte ; reading byte (8 bits)
      bytes ; reading bytes (bytes "asds"), (bytes "asas" value), (bytes #(1 2 3)), (bytes #(1 2 3) value)

      rune ; like byte, but utf-8 encoded
      runes ; instead of "word"

      line ; linux-style line

      maybe
      times
      either
      any-of

      greedy* ; 0+
      greedy+ ; 1+
      lazy*   ; 0+
      lazy+   ; 1+

      epsilon ; just return a value, without input stream consumption
      eof)    ; end-of-file handle

   (import
      (scheme core)
      (owl math)
      (owl string)
      (only (scheme vector) vector->list)
      (only (scheme bytevector) bytevector->list)
      (owl unicode)
      (owl lazy))

   (begin

      (define (char? o) (eq? (type o) type-value+)) ; * internal
      (define eof-error #eof) ; "end of input"

      ;; bactrtrack function : l r fp why
      ; l - stream
      ; r - ?
      ; p - ?
      ; why - reason
      (define (backtrack l r p why)
         (if (null? l)
            (values #f r p why) ;; final outcome if error
            (let ((hd (car l))) ;; rollback
               (if (char? hd)
                  (backtrack (cdr l) (cons hd r) p why)
                  (hd (cdr l) r p why)))))

      ; todo: bind instead of eval?
      (define-syntax let-parse*
         (syntax-rules (!! unless eval backtrack)
            ((let-parse* 42 l r p ok ((val (eval term)) . rest) body)
               (let ((val term))
                  (let-parse* 42 l r p ok rest body)))
            ((let-parse* 42 l r p ok ((val parser) . rest) body)
               (parser l r p
                  (λ (l r p val)
                     (let-parse* 42 l r p ok rest body))))
            ((let-parse* 42 l r p ok () body)
               (ok l r p body))

            ; verification primitives: !!, unless
            ((let-parse* 42 l r p ok ((!! term msg) . rest) body)
               (if term
                  (let-parse* 42 l r p ok rest body)
                  (backtrack l r p msg)))
            ((let-parse* 42 l r p ok ((unless term msg) . rest) body)
               (if term
                  (let-parse* 42 l r p ok rest body)
                  (backtrack l r p msg)))
            
            ((let-parse* ((a . b) ...) body)
               (λ (l r p ok)
                  (let-parse* 42 l r p ok ((a . b) ...) body)))
            ((let-parse* ((a . b) ...) something . rest)
               (let-parse* ((a . b) ...) (begin something . rest)))))

      (define-syntax any-of
         (syntax-rules (either)
            ((any-of a) a)
            ((any-of a b) (either a b))
            ((any-of a b . c) (either a (any-of b . c)))))

      (define (either a b)
         (λ (l r p ok)
            (a (cons (λ (l r fp why) (b l r p ok)) l) r p ok)))

      ; -------------------------------------------------
      (define (epsilon val)
         (λ (l r p ok)
            (ok l r p val)))

      (define (eof l r p ok)
         (let loop ((r r))
            (cond
               ((null? r) (ok l r p #eof))
               ((pair? r) (backtrack l r p '("expected eof")))
               (else
                  (loop (r))))))

      ; -------------------------------------------------
      ; read byte from the data stream
      (define byte
         (define (byte l r p ok)
            (let loop ((r r))
               (cond
                  ((null? r) (backtrack l r p eof-error))
                  ((pair? r) (ok (cons (car r) l) (cdr r) (+ p 1) (car r)))
                  (else      (loop (r))))))

         (case-lambda
            ; (value (byte)), convenient way to get a byte
            (() byte)
            ; (value byte), legacy but a bit faster behavior
            ((l r p ok)
               (byte l r p ok))
            ; (value (byte f)), conditional parsing
            ((f)
               (if (value? f)
                  ; formerly (value (imm value))
                  (let-parse* (
                        (x byte)
                        (unless (eq? x f) `("expected" ,f)))
                     x)
                  ; formerly (value (byte-if predicate))
                  (let-parse* (
                        (x byte)
                        (unless (f x) `("bad byte" ,x)))
                     x) ))
         ))

      ; formerly (word ...)
      ;  attention, will not handle non-ansi strings!
      (define bytes
         (define (bytes x ret)
            (let ((sample ((cond
                        ((string? x) string->bytes) ; utf8 bytes
                        ((vector? x) vector->list)
                        ((bytevector? x) bytevector->list)) x)))
               (\ (l r p ok)
                  (let loop ((l l) (r r) (p p) (left sample))
                     (cond
                        ((null? left)
                           (ok l r p ret))
                        ((null? r)
                           (backtrack l r p eof-error))
                        ((pair? r)
                           (if (eq? (car r) (car left))
                              (loop (cons (car r) l) (cdr r) (+ p 1) (cdr left))
                              (backtrack l r p `("expected" ,(car left)))))
                        (else
                           (loop l (r) p left)))))))
      
         (case-lambda
            ((word)
               (bytes word word))
            ((word f)
               (bytes word f))
         ))

      ;
      ; utf-8 handling -------------------------------------------------------------
      ; #b10xxxxxx
      (define extension-byte
         (let-parse* (
               (b byte)
               (unless (eq? (vm:and b #b11000000) #b10000000) "Bad extension byte"))
            b))

      (define (byte-between below above) ; without borders, internal fast comparator
         (byte (\ (x)
            (and (less? below x) (less? x above)))))

      ; get an utf-8 character (rune)
      (define rune
         (define rune
            (any-of
               (byte (\ (x) (less? x 128)))
               (let-parse* (
                     (a (byte-between 127 224))
                     ; note: two byte sequence #b11000000 10xxxxxx is invalid
                     (unless (not (eq? a #b11000000)) "invalid utf-8 stream")
                     (b extension-byte))
                  (two-byte-point a b))
               (let-parse* (
                     (a (byte-between 223 240))
                     ; note: three byte sequence #b11100000 10xxxxxx 10xxxxxx is valid!
                     (b extension-byte) (c extension-byte))
                  (three-byte-point a b c))
               (let-parse* (
                     (a (byte-between 239 280))
                     ; note: four byte sequence #b11110000 10xxxxxx 10xxxxxx 10xxxxxx is valid!
                     (b extension-byte) (c extension-byte) (d extension-byte))
                  (four-byte-point a b c d))))

         (case-lambda
            ; (value (rune)), convenient way to get a rune
            (() rune)
            ; (value rune), legacy but a bit faster behavior
            ((l r p ok)
               (rune l r p ok))
            ; (value (rune f)), conditional parsing
            ((f)
               (if (value? f)
                  ; formerly (value (imm value))
                  (let-parse* (
                        (x rune)
                        (unless (eq? x f) `("expected" ,f)))
                     x)
                  ; formerly (value (rune-if predicate))
                  (let-parse* (
                        (x rune)
                        (unless (f x) `("bad rune" ,x)))
                     x) ))
         ))

      (define runes bytes) ; reuse bytes, will not work with vectors and bytevectors

      ; ------------------------

      (define (maybe x val)
         (either
            x
            (epsilon val)))

      (define (times n parser)
         (lambda (l r p ok)
            (if (eq? n 0)
               (ok l r p #n)
               (let loop ((l l) (r r) (p p) (n n) (v #null))
                  (cond
                     ((null? r) (backtrack l r p eof-error))
                     ((pair? r)
                        (let* ((l r p val (parser l r p (lambda (l r p v) (values l r p v)))))
                           (if (eq? n 1)
                              (ok l r p (reverse (cons val v)))
                              (loop l r p (- n 1) (cons val v)))))
                     (else
                        (loop l (r) p n v)))))))

      ; line up to #\newline (with #\newline) or to the end of file
      ; not handling CR
      (define (line l r p ok) ; always runs ok
         (let loop ((l l) (r r) (p p) (v #null))
            (cond
               ((null? r) ; eof
                  (ok l r p v))
               ((pair? r)
                  (if (eq? (car r) #\newline)
                     (ok (cons (car r) l) (cdr r) (+ p 1) (reverse v))
                     (loop (cons (car r) l) (cdr r) (+ p 1) (cons (car r) v))))
               (else
                  (loop l (r) p v)))))

      ; -- lazy/greedy ----------------------------------------

      ; greedy...
      (define (drop l x)
         (cond
            ((eq? (car l) x)
               (cdr l))
            ((char? (car l))
               (cons (car l) (drop (cdr l) x)))
            (else
               (drop (cdr l) x))))


      (define (greedy-star-vals parser vals)
         (λ (l r p ok)
            (define (bt l r fp why)
               (ok l r p (reverse vals)))
            (parser
               (cons bt l)
               r
               p
               (λ (l r p val)
                  ((greedy-star-vals parser (cons val vals))
                     (drop l bt) r p ok)))))

      (define (greedy* v)
         (greedy-star-vals v #null))

      (define (greedy+ a)
         (let-parse* (
               (first a)
               (rest (greedy* a)))
            (cons first rest)))


      (define (lazy-star-vals parser vals)
         (λ (l r p ok)
            (define (bt l r fp why)
               (parser l r fp (λ (l r p val)
                  ((lazy-star-vals parser (cons val vals))
                     l r p ok))))
            (ok (cons bt l) r p (reverse vals))))

      (define (lazy* parser)
         (lazy-star-vals parser #null))

      (define (lazy+ a)
         (let-parse* (
               (first a)
               (rest (lazy* a)))
            (cons first rest)))


      ; -------------------------------------------------------
      (define (ok l r p v) ; internal
         (values l r p v))

      ; returns pair '(value . stream-tail)
      (define let-parse
         (define (let-parse parser data fail)
            (let* ((l r p val (parser #null data 0 ok)))
               (if l
                  (cons val r) ; '(val . #null) in case of full match
                  (cons fail #f)))) ; fail
         (case-lambda
            ((parser data) (let-parse parser data #f))
            ((parser data fail) (let-parse parser data fail))))

      ; returns value
      (define parse
         (define (parse parser data fail)
            (let* ((l r p val (parser #null data 0 ok)))
               (if l val fail)))
      
      (case-lambda
         ((parser data)
            (parse parser data #f))
         ; five arguments is deprecated:
         ((parser data fail)
            (parse parser data fail))))

))

