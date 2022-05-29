(define-library (owl parse)
   (export
      let-parse*
      backtrack

      epsilon ;ε

      maybe
      times

      either ;either!
      any-of ;any-of!

      greedy* greedy+
      lazy* lazy+

      byte
      byte-if
      byte-between

      imm ; todo: rename
      word

      rune
      rune-if

      ;; star
      ;; plus
      ;; parse-head
      parse                ;; parser data fail-val → result | fail-val (if no full match)
      try-parse
      ;; first-match          ;; parser data fail-val → result data'

      ;; byte-stream->exp-stream
      ;; fd->exp-stream
      ;; file->exp-stream
      ;; silent-syntax-fail
      ;; resuming-syntax-fail ;; error-msg → _

      ;; backward compatibility
      let-parses
      get-epsilon ;ε

      get-either ;either!
      get-maybe
      get-any-of ;any-of!

      get-greedy*
      get-greedy+

      get-byte
      get-byte-if
      get-byte-between

      get-imm ; todo: rename
      get-word

      get-rune
      get-rune-if

      )

   (import
      (scheme base)
      (owl unicode)
      (owl lazy))

   (begin

      ;; Why = #(pos message <rest>)
      ;;
      ;; (parser l r p ok) ; left, right, position, ok
      ;;   → (ok l' r' p' val) | (backtrack l r p why)
      ;   ... → l|#f r p' result|error

      (define-syntax let-parse*
         (syntax-rules (verify eval backtrack)
            ((let-parse* 42 l r p ok ((val (eval term)) . rest) body)
               (let ((val term))
                  (let-parse* 42 l r p ok rest body)))
            ((let-parse* 42 l r p ok ((val parser) . rest) body)
               (parser l r p
                  (λ (l r p val)
                     (let-parse* 42 l r p ok rest body))))
            ((let-parse* 42 l r p ok () body)
               (ok l r p body))
            ((let-parse* 42 l r p ok ((verify term msg) . rest) body)
               (if term
                  (let-parse* 42 l r p ok rest body)
                  (backtrack l r p msg)))
            ((let-parse* ((a . b) ...) body)
               (λ (l r p ok)
                  (let-parse* 42 l r p ok ((a . b) ...) body)))
            ((let-parse* ((a . b) ...) first . rest)
               (let-parse* ((a . b) ...) (begin first . rest)))))


      ;; bactrtrack function : l r fp why
      ; l - stream
      ; r - ?
      ; p - ?
      ; why - reason
      (define (backtrack l r p why)
         (if (null? l)
            (values #f r p why) ;; final outcome if error
            (let ((hd (car l)))
               (if (char? hd)
                  (backtrack (cdr l) (cons hd r) p why)
                  (hd (cdr l) r p why)))))

      (define eof-error #eof) ;"end of input")
      (define wrong-char "syntax error")

      (define (epsilon val)
         (λ (l r p ok)
            (ok l r p val)))

      (define ε epsilon)

      (define (either a b)
         (λ (l r p ok)
            (a (cons (λ (l r fp why) (b l r p ok)) l) r p ok)))

      ;; (define (either! a b)
      ;;    (λ (l r p ok)
      ;;       (a
      ;;          (cons
      ;;             (λ (l r fp why)
      ;;                (if (= fp p) ;; nothing matched
      ;;                   (b l r p ok)
      ;;                   (backtrack l r fp why)))
      ;;             l)
      ;;          r p ok)))

      (define (maybe x val)
         (either
            x
            (epsilon val)))

      (define (times n parser)
         (lambda (l r p ok)
            (let loop ((l l) (r r) (p p) (n n) (v #null))
               (cond
                  ((null? r) (backtrack l r p eof-error))
                  ((pair? r)
                     (let* ((l r p val (parser l r p (lambda (l r p v) (values l r p v)))))
                        (if (eq? n 1)
                           (ok l r p (reverse (cons val v)))
                           (loop l r p (- n 1) (cons val v)))))
                  (else
                     (loop l (r) p n v))))))


      (define-syntax any-of
         (syntax-rules ()
            ((any-of a) a)
            ((any-of a b) (either a b))
            ((any-of a b . c) (either a (any-of b . c)))))

      ;; (define-syntax one-of!
      ;;    (syntax-rules ()
      ;;       ((one-of! a) a)
      ;;       ((one-of! a b) (either! a b))
      ;;       ((one-of! a b . c) (either! a (one-of! b . c)))))

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

      ;; (define star! greedy-star)
      ;; (define plus! greedy-plus)

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

      ; actual readings

      (define (byte l r p ok)
         (cond
            ((null? r) (backtrack l r p eof-error))
            ((pair? r) (ok (cons (car r) l) (cdr r) (+ p 1) (car r)))
            (else      (byte l (r) p ok))))

      (define (byte-if pred)
         ; question: why not a simple version?
         (let-parse* (
               (b byte)
               (verify (pred b) `("bad character '" ,(if (> b 31) (string b) " ") "' (" ,b ")")))
            b))
         ;; (λ (l r p ok)
         ;;    (cond
         ;;       ((null? r)
         ;;          (backtrack l r p eof-error))
         ;;       ((pair? r)
         ;;          (let* ((x xt r))
         ;;             (if (pred x)
         ;;                (ok (cons x l) xt (+ p 1) x)
         ;;                (backtrack l r p wrong-char))))
         ;;       (else
         ;;          ((byte-if pred) l (r) p ok)))))

      (define (byte-between below above)
         (byte-if
            (λ (x)
               (and (less? below x) (less? x above)))))

      ;; (define (peek-byte pred)
      ;;    (λ (l r p ok)
      ;;       (cond
      ;;          ((null? r)
      ;;             (backtrack l r p eof-error))
      ;;          ((pair? r)
      ;;             (if (pred (car r))
      ;;                (ok l r p (car r))
      ;;                (backtrack l r p wrong-char)))
      ;;          (else
      ;;             ((peek-byte pred) l (r) p ok)))))

      (define (imm x)
         ; todo: test which version is faster?
         (let-parse* (
               (b byte)
               (verify (eq? b x) `(expected ,x)))
            x))
         ;; (λ (l r p ok)
         ;;    (cond
         ;;       ((null? r)
         ;;          (backtrack l r p eof-error))
         ;;       ((pair? r)
         ;;          (if (eq? (car r) x)
         ;;             (ok (cons x l) (cdr r) (+ p 1) x)
         ;;             (backtrack l r p `(expected byte ,x))))
         ;;       (else
         ;;          ((imm x) l (r) p ok)))))


      ;; (define (seq a b)
      ;;    (λ (l r p ok)
      ;;       (a l r p
      ;;          (λ (l r p av)
      ;;             (b l r p
      ;;                (λ (l r p bv)
      ;;                   (ok l r p (cons av bv))))))))

      ;; (define (star-vals a vals)
      ;;    (λ (l r p ok)
      ;;       (a
      ;;          (cons
      ;;             (λ (l r fp why)
      ;;                 (ok l r p (reverse vals)))
      ;;             l)
      ;;          r
      ;;          p
      ;;          (λ (l r p val)
      ;;             ((star-vals a (cons val vals)) l r p ok)))))

      ;; (define (C f y) (λ (x) (f x y)))

      ;; (define star
      ;;    (lambda (x)
      ;;       (star-vals x #null))

      ;; (define (plus parser)
      ;;    (parses
      ;;       ((a parser)
      ;;        (as (star parser)))
      ;;       (cons a as)))

      (define (word s val)
         ; question: why not a limple version?
         ; todo: remake
         (let ((bytes (string->bytes s)))
            (λ (l r p ok)
               (let loop ((l l) (r r) (p p) (left bytes))
                  (cond
                     ((null? left)
                        (ok l r p val))
                     ((null? r)
                        (backtrack l r p eof-error))
                     ((pair? r)
                        (if (eq? (car r) (car left))
                           (loop (cons (car r) l) (cdr r) (+ p 1) (cdr left))
                           (backtrack l r p `(expected ,(car left)))))
                     (else
                        (loop l (r) p left)))))))

      ; todo: word-ci

      ; #b10xxxxxx
      (define extension-byte
         (let-parse* (
               (b byte)
               (verify (eq? (vm:and b #b11000000) #b10000000) "Bad extension byte"))
            b))

      ; get an utf-8 character
      (define rune
         (any-of
            (byte-if (λ (x) (less? x 128)))
            (let-parse* (
                  (a (byte-between 127 224))
                  ; note: two byte sequence #b11000000 10xxxxxx is invalid
                  (verify (not (eq? a #b11000000)) "invalid utf-8 stream")
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

      (define (rune-if pred)
         (let-parse* (
               (rune rune)
               (verify (pred rune) `(bad rune ,rune)))
            rune))


      (define (parser-succ l r p v)
         (values l r p v))


      ;; (define (parse-head parser ll def)
      ;;    (let* ((l r p val (parser #n ll 0 parser-succ)))
      ;;       (if l (cons val r) def)))

      ;; ;; computes rest of parser stream
      ;; (define (silent-syntax-fail val)
      ;;    (λ (cont ll msg) val))

      ;; (define (fast-forward ll)
      ;;    (if (pair? ll)
      ;;       (fast-forward (cdr ll))
      ;;       ll))

      ;; (define (whitespace? ll)
      ;;    (cond
      ;;       ((null? ll) #t)
      ;;       ((not (pair? ll)) #f)
      ;;       ((memq (car ll) '(#\newline #\space #\return #\tab))
      ;;          (whitespace? (cdr ll)))
      ;;       (else #f)))

      ;; (define (resuming-syntax-fail error-reporter)
      ;;    (λ (cont ll msg)
      ;;       ;; this is a bit of a hack
      ;;       ;; allow common whitespace at end of input, because parsers typically define structure
      ;;       ;; only up to last byte byte needed for recognition in order to avoid blocking
      ;;       (let ((rest (fast-forward ll)))
      ;;          (if (and (null? rest) (whitespace? ll))
      ;;             (cont #n)
      ;;             (begin
      ;;                (error-reporter msg)
      ;;                (cont rest))))))

      ;; (define (stopping-syntax-fail error-reporter)
      ;;    (λ (cont ll msg)
      ;;       (let ((rest (fast-forward ll)))
      ;;          (if (and (null? rest) (whitespace? ll))
      ;;             (cont #n)
      ;;             (begin
      ;;                (error-reporter msg)
      ;;                (cont rest))))))

      ;; ;; ll parser (ll r val → ?) → ll
      ;; (define (byte-stream->exp-stream ll parser fail)
      ;;    (λ ()
      ;;       (let* ((lp r p val (parser #n ll 0 parser-succ)))
      ;;          (cond
      ;;             (lp ;; something parsed successfully
      ;;                (lcons val (byte-stream->exp-stream r parser fail)))
      ;;             ((null? r) ;; end of input
      ;;                ;; typically there is whitespace, so this does not happen
      ;;                #n)
      ;;             ((function? fail)
      ;;                (fail
      ;;                   (λ (ll) (byte-stream->exp-stream ll parser fail))
      ;;                    r val))
      ;;             (else
      ;;                #n)))))

      (define (try-parse parser data unused)
         (let* ((l r p val (parser #null data 0 parser-succ)))
            (when l
               (cons val r)))) ; '(val . #null) in case of full match

      (define (parse parser data unused-path errmsg fail-val) ; todo: use path
         (let loop ((try (λ () (parser #null data 0 parser-succ))))
            (let* ((l r p val (try)))
               (cond
                  ((not l)
                     fail-val)
                  ((lpair? r) => (λ (r)
                     ;; trailing garbage
                     fail-val))
                  (else
                     ;; full match
                     val)))))

      ;; (define (first-match parser data fail-val)
      ;;    (let loop ((try (λ () (parser #n data 0 parser-succ))))
      ;;       (let* ((l r p val (try)))
      ;;           (cond
      ;;             ((not l)
      ;;                (values fail-val r p))
      ;;             (else
      ;;                (values val r p))))))


      (define get-epsilon epsilon)

      (define get-either either)
      (define get-maybe maybe)
      (define-syntax get-any-of
         (syntax-rules ()
            ((get-any-of a) a)
            ((get-any-of a b) (either a b))
            ((get-any-of a b . c) (either a (get-any-of b . c)))))

      (define get-greedy* greedy*)
      (define get-greedy+ greedy+)

      (define get-byte byte)
      (define get-byte-if byte-if)
      (define get-byte-between byte-between)

      (define get-imm imm); todo: rename
      (define get-word word)

      (define get-rune rune)
      (define get-rune-if rune-if)

      (define-syntax let-parses
         (syntax-rules (verify eval backtrack)
            ((let-parses 42 l r p ok ((val (eval term)) . rest) body)
               (let ((val term))
                  (let-parses 42 l r p ok rest body)))
            ((let-parses 42 l r p ok ((val parser) . rest) body)
               (parser l r p
                  (λ (l r p val)
                     (let-parses 42 l r p ok rest body))))
            ((let-parses 42 l r p ok () body)
               (ok l r p body))
            ((let-parses 42 l r p ok ((verify term msg) . rest) body)
               (if term
                  (let-parses 42 l r p ok rest body)
                  (backtrack l r p msg)))
            ((let-parses ((a . b) ...) body)
               (λ (l r p ok)
                  (let-parses 42 l r p ok ((a . b) ...) body)))
            ((let-parses ((a . b) ...) first . rest)
               (let-parses ((a . b) ...) (begin first . rest)))))

))

