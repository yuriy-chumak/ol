;; todo: remove implicit UTF-8 conversion from parser and move to a separate pass

(define-library (lang sexp)
   (export
      sexp-parser sexp
      ;get-sexps       ;; greedy* get-sexp
      ;; string->sexp
      ;bytevector->sexps

      list->sexps
      list->number

      ; parser
      number get-number
      whitespace
      whitespace-or-comment

      get-sexps
      get-padded-sexps

      fd->exp-stream
      file->exp-stream)

   (import
      (scheme base)

      (owl parse)
      (owl math)
      (owl math fp)
      (owl string)
      (owl list)
      (owl ff)
      (owl lazy)
      (owl io) ; testing
      (owl unicode)
      (only (lang intern) string->uninterned-symbol)
      (only (owl regex) get-sexp-regex))

   (begin

      (define (between? lo x hi) ; fast version of (<= lo x hi))
         (and
            (or (less? lo x) (eq? lo x))
            (or (less? x hi) (eq? x hi))))

      (define (left a b) a)
      (define (putT a) (cons a #T))


      (define uppercase-chars (alist->ff
         (map putT (iota (- #\Z #\A -1) #\A)) ))
      (define lowercase-chars (alist->ff
         (map putT (iota (- #\z #\a -1) #\a)) ))

      (define alphabetic-chars (ff-union #false
         uppercase-chars
         lowercase-chars)) ; will not intersect

      (define numeric-chars (alist->ff
         (map putT (iota (- #\9 #\0 -1) #\0)) ))

      (define whitespace-chars (alist->ff
         (map putT (list #\tab #\newline #\space #\return)) ))


      (define symbol-lead-chars (ff-union left
         alphabetic-chars
         (alist->ff ; no ', digits and /
            (map putT (string->runes "!$%&*+-:<=>?@^_~\\"))
      )))
      (define symbol-lead-chars/
         (put symbol-lead-chars #\/ #T))

      (define symbol-chars/ (ff-union left
         symbol-lead-chars
         numeric-chars
         (alist->ff
            (map putT (string->runes "'`#/\""))  ;; we can use ['`#/"] as part of symbol names
      )))

      (define (symbol-lead-char? n)
         (symbol-lead-chars n (less? 126 n)))
      (define (symbol-lead-char/? n)
         (symbol-lead-chars/ n (less? 126 n)))
      (define (symbol-char/? n)
         (symbol-chars/ n (less? 126 n)))

      ; simple symbols (without "/" and "|"), easy case for speedup
      (define simple-symbol
         (let-parse* (
               (s1 (rune-if symbol-lead-char?))
               (s2 (rune-if symbol-lead-char?))
               (tail (greedy* (rune-if symbol-char/?))))
            (string->uninterned-symbol (runes->string (cons* s1 s2 tail)))))

      ; full featured symbols (with "|" and "/"), must be used after regex parser
      ;  (that may contain "/" and "|" both)
      (define symbol
         (either
            (let-parse* (
                  (head (rune-if symbol-lead-char/?))
                  (tail (greedy* (rune-if symbol-char/?))))
               (string->uninterned-symbol (runes->string (cons head tail))))
            (let-parse* (
                  (skip (imm #\|))
                  (chars (greedy* (rune-if (λ (x) (not (eq? x #\|))))))
                  (skip (imm #\|)))
               (string->uninterned-symbol (runes->string chars)))))

      ;; 
      (define digit-values (alist->ff
         (append
            (map (lambda (d i) (cons d i)) (iota 10 #\0) (iota 10 0))  ;; 0-9
            (map (lambda (d i) (cons d i)) (iota  6 #\A) (iota 6 10))  ;; A-F
            (map (lambda (d i) (cons d i)) (iota  6 #\a) (iota 6 10))  ;; a-f
      )))

      (define (digit-char? base)
         (if (eq? base 10)
            (λ (n) (between? #\0 n #\9))
            (λ (n) (< (digit-values n 100) base)))) ; base should be less than 100

      (define (bytes->number digits base)
         (fold
            (λ (n digit)
               (let ((d (digit-values digit #false)))
                  (cond
                     ((or (not d) (>= d base))
                        (runtime-error "bad digit " digit))
                     (else
                        (+ (* n base) d)))))
            0 digits))

      (define signer
         (let-parse*
               ((s (any-of (imm #\+) (imm #\-) (epsilon #\+))))
            (if (eq? s #\+)
               (λ (x) x)
               negate)))


      (define (natural base)
         (let-parse*
               ((digits (greedy+ (byte-if (digit-char? base)))))
            (bytes->number digits base)))

      (define (integer base)
         (let-parse*
               ((sign signer)
                (n (natural base)))
            (sign n)))

      ;; → n, to multiply with
      ; r7rs accepts only exponent in base 10
      (define exponent
         (either
            (let-parse*
                  ((skip (imm #\e))
                   (pow (integer 10)))
               (expt 10 pow))
            (epsilon 1)))

      ;; separate parser with explicitly given base for string->number
      (define (number-in-base base)
         (let-parse* (
               (sign signer) ;; default + <- could allow also an optional base here
               (num (integer base))
               (tail (either ;; optional after dot part be added
                        (let-parse* (
                              (skip (imm #\.))
                              (digits (greedy* (byte-if (digit-char? base)))))
                           (/ (bytes->number digits base)
                              (expt base (length digits))))
                        (epsilon 0)))
                (pow exponent))
            (sign (* (+ num tail) pow))))

      ;; a sub-rational (other than as decimal notation) number
      (define bases {
         #\b   2
         #\o   8
         #\d  10
         #\x  16
      })
      (define base
         (any-of
            (let-parse*
                  ((skip (imm #\#))
                   (char (byte-if (λ (x) (getf bases x)))))
               (getf bases char))
            (epsilon 10)))

      (define number-unit
         (let-parse*
               ((b base) ;; default 10
                (val (number-in-base b)))
            val))

      ;; anything up to a rational
      (define rational
         (let-parse* (
               (n number-unit)
               (m (either
                     (let-parse* (
                           (skip (imm #\/))
                           (m number-unit))
                        (/ n m))
                     (epsilon n))))
            m))

      ; IEEE 754
      ;(setq |+inf.0| (fdiv 1 0)) ; 1 / 0 = +infin
      ;(setq |-inf.0| (flog 0))   ; log( 0 ) = -infin
      ;(setq |+nan.0| (fsqrt -1)) ; sqrt( -1 ) = NaN

      (define real
         (any-of
               rational ; typically this is it
               (word "+inf.0" +inf.0)
               (word "-inf.0" -inf.0)
               (word "+nan.0" +nan.0)
               ; #e/#i (exact/inexact)
               (let-parse* (
                     (conv (either
                              (word "#i" inexact)
                              (word "#e" exact)))
                     (num rational))
                  (conv num))))


      (define imaginary-part
         (let-parse* (
               (s signer)
               (im (either real (epsilon 1))) ; we also want 0+i
               (/ (imm #\i)))
            (s im)))

      (define number
         (let-parse* (
               (r real)
               (im (either imaginary-part (epsilon 0))))
            (if (zero? im)
               r
               (complex r im))))

      ;; #!<string>\n parses to '(hashbang <string>)
      (define hashbang
         (let-parse*(
               (hash (imm #\#))
               (bang (imm #\!))
               (line rest-of-line))
            (list 'quote (list 'hashbang (list->string line)))))

      ;; skip everything up to |#
      (define (block-comment)
         (either
            (let-parse*
               ((skip (imm #\|))
                (skip (imm #\#)))
               'comment)
            (let-parse*
               ((skip byte)
                (skip (block-comment)))
               skip)))

      ;; ; List of Unicode Characters of Category "Space Separator" (Zs, Space_Separator)
      ;; ; note: no zero-width spaces will use (U+180E, U+200B, U+FEFF)
      ;; (define (putt ff key) (put ff key #true))
      ;; (setq spaces (fold putt {} '(
      ;;    #\space ; Space, SP
      ;;    #x000A0 ; No-Break Space
      ;;    #x01680 ; Ogham Space Mark
      ;;    #x02000 ; En Quad
      ;;    #x02001 ; Em Quad
      ;;    #x02002 ; En Space
      ;;    #x02003 ; Em Space
      ;;    #x02004 ; Three-Per-Em Space
      ;;    #x02005 ; Four-Per-Em Space
      ;;    #x02006 ; Six-Per-Em Space
      ;;    #x02007 ; Figure Space
      ;;    #x02008 ; Puctuation Space
      ;;    #x02009 ; Thin Space
      ;;    #x0200A ; Hair Space
      ;;    #x0202F ; Narrow No-Break Space (NNBSP)
      ;;    #x0205F ; Mediaum Mathematical Space (MMSP)
      ;;    #x03000 ; Ideographic Space, used by oriental languages
      ;; )))
      ;; (setq whitespaces (fold putt spaces
      ;;    '(#\tab #\newline #\return)))

      (define (char-whitespace? ch)
         (whitespace-chars ch #false))

      (define whitespace
         (byte-if char-whitespace?))

      (define whitespace-or-comment
         (any-of
            whitespace
            (let-parse*
               ((skip (imm #\;))
                (skip rest-of-line))
               'comment)
            (let-parse*
               ((skip (imm #\#))
                (skip (imm #\|))
                (skip (block-comment)))
               'comment)))

      (define maybe-whitespace (greedy* whitespace-or-comment))

      (define (list-of parser)
         (let-parse*
            ((lp (imm #\())
             (things
               (greedy* parser))
             (skip maybe-whitespace)
             (tail
               (either
                  (let-parse* ((rp (imm #\)))) null)
                  (let-parse*
                     ((dot (imm #\.))
                      (fini parser)
                      (skip maybe-whitespace)
                      (skip (imm #\))))
                     fini))))
            (if (null? tail)
               things
               (append things tail))))

      (define quoted-values {
         #\a  #x0007
         #\b  #x0008
         #\t  #x0009
         #\n  #x000a
         #\v  #x000b
         #\f  #x000c
         #\r  #x000d
         #\e  #x001B
         #\0  #x0000
         #\"  #x0022
         #\\  #x005c
      })

      (define quoted-string-char
         (let-parse*
            ((skip (imm #\\))
             (char
               (either
                  (let-parse*
                     ((char (byte-if (λ (byte) (quoted-values byte #f)))))
                     (getf quoted-values char))
                  (let-parse*
                     ((skip (imm #\x))
                      (hexes (greedy+ (byte-if (digit-char? 16))))
                      (skip (imm #\;)))
                     (bytes->number hexes 16)))))
            char))

      (define string
         (let-parse*
            ((skip (imm #\"))
             (chars
               (greedy*
                  (either
                     quoted-string-char
                     (rune-if (lambda (x) (not (has? '(#\" #\\) x)))))))
             (skip (imm #\")))
            (runes->string chars)))

      (define quotations {
         #\'  'quote
         #\,  'unquote
         #\`  'quasiquote
         'splice 'unquote-splicing
      })

      (define (quoted parser)
         (let-parse*
            ((type
               (either
                  (let-parse* ((_ (imm #\,)) (_ (imm #\@))) 'splice) ; ,@
                  (byte-if (λ (x) (get quotations x #false)))))
             (value parser))
            (list (get quotations type #false) value)))

      (define named-char
         (any-of
            (word "null"      #\null)      ; 0
            (word "alarm"     #\alarm)     ; 7
            (word "backspace" #\backspace) ; 8
            (word "tab"       #\tab)       ; 9
            (word "newline"   #\newline)   ;10
            (word "vtab"      #\vtab)      ;11
            (word "formfeed"  #\formfeed)  ;12
            (word "return"    #\return)    ;13
            (word "escape"    #\escape)    ;27
            (word "space"     #\space)     ;32
            (word "percent"   #\percent)   ;37
            (word "ampersand" #\ampersand) ;38
            (word "comma"     #\comma)     ;44
            (word "slash"     #\slash)     ;47
            (word "colon"     #\colon)     ;58
            (word "semicolon" #\semicolon) ;59
            (word "backslash" #\backslash) ;92
            (word "delete"    #\delete))) ;127

      ;; fixme: add named characters #\newline, ...
      (define quoted-char
         (let-parse*
            ((skip (imm #\#))
             (skip (imm #\\))
             (codepoint (either named-char rune)))
            codepoint))

      ;; #...
      (define (special-word sexp)
         (any-of
            (word "..." '...)
            (let-parse* (
                  (skip (imm #\#))
                  (val (any-of
                        (word "true"  #true)
                        (word "false" #false)
                        (word "null"  #null)    ; empty (), system constant

                        ; #ff(...)
                        (let-parse* (
                              (-- (imm #\f))
                              (-- (imm #\f))
                              (-- (imm #\())
                              (things
                                 (greedy* (sexp)))
                              (-- maybe-whitespace)
                              (-- (imm #\))) )
                           (if (null? things)
                              #empty
                           else
                              (list 'alist->ff (cons 'list (map (lambda (pair)
                                 (list 'quote pair)) things)))))
                        (word "empty" #empty)   ; empty #ff(), system constant

                        ; #u8(... only bytes ...)
                        (let-parse* (
                              (-- (imm #\u))
                              (-- (imm #\8))
                              (-- (imm #\())
                              (things
                                 (greedy* (let-parse* (
                                       (-- maybe-whitespace)
                                       (base base)
                                       (number (natural base)))
                                    number)))
                              (-- maybe-whitespace)
                              (-- (imm #\))) )
                           (list 'make-bytevector (cons 'list things)))

                        ; end-of-file
                        (word "eof"   #eof)

                        ; сокращения
                        (word "t" #true)
                        (word "f" #false)
                        (word "T" #true)
                        (word "F" #false)
                        (word "n" #null)
                        (word "N" #null)
                        (word "e" #empty)
                        (word "E" #empty) )))
               val)))


      (define (intern-symbols sexp)
         (cond
            ((symbol? sexp)
               (string->symbol (ref sexp 1)))
            ((pair? sexp)
               (cons (intern-symbols (car sexp)) (intern-symbols (cdr sexp))))
            (else sexp)))

      (define (vector-of parser)
         (let-parse* (
               (qv (either
                  ; #(), quoting all values, scheme syntax
                  (let-parse* (
                        (* (imm #\#))
                        (* (imm #\())
                        (things
                           (greedy* parser))
                        (* maybe-whitespace)
                        (* (imm #\))))
                     (cons 'quote things))
                  ; [], not quoting values (except used ' or `), ol syntax
                  (let-parse* (
                        (q (any-of
                              (word "'" 'quote)
                              (word "`" 'quasiquote)
                              (epsilon #false)))
                        (* (imm #\[))
                        (things
                           (greedy* parser))
                        (* maybe-whitespace)
                        (* (imm #\])))
                     (cons q things)))))
            (let*((q things qv))
               (if (null? things) ; only lists can be in parsed expression
                  (list 'make-vector #null)
                  (if q
                     (list 'make-vector (list q things))
                     (list 'make-vector (cons 'list things)))))))

      (define (ff-of parser)
         (let-parse* (
               (q (any-of
                     (word "'" 'quote)
                     (word "`" 'quasiquote)
                     (epsilon #false)))
               (? (imm #\{))
               (things
                  (greedy* parser))
               (? maybe-whitespace)
               (? (imm #\})))
            (if (null? things) ; only lists can be in parsed expression
               (list 'make-ff #null)
               (if q
                  (list 'make-ff (list q things))
                  (list 'make-ff (cons 'list things))))))

      ; returns uninterned symbols
      (define (sexp)
         (let-parse* (
               (skip maybe-whitespace)
               (val (any-of
                        ; lists
                        (list-of (sexp)) ; todo: move below
                        ; simple types
                        number
                        simple-symbol
                        (special-word sexp)
                        string
                        quoted-char
                        get-sexp-regex ; before symbols, which also may have "/" and "|"
                        symbol        ; 
                        ; containers
                        (vector-of (sexp))
                        (ff-of (sexp))
                        (quoted (sexp))
                        ; eof
                        (byte-if eof?))))
            val))

      (define hashbang+sexp
         (let-parse* (
               (-- (maybe hashbang #f));; skip leading "#! ...\n"
               (value (sexp)))
            value))

      (define (ok? x) (eq? (ref x 1) 'ok))
      (define (ok exp env) ['ok exp env])
      (define (fail reason) ['fail reason])

      ; returns interned symbols
      (define sexp-parser
         ;; old code:
         ;; (let-parse* (
         ;;       (s-exp (sexp)))
         ;;    (intern-symbols s-exp)))
         (λ (l r p ok)
            (let* ((l r p val (hashbang+sexp l r p
                                 ; ok
                                 (λ (l r p val)
                                    (ok l r p (intern-symbols val))))))
               ; well, we can do a trick:
               ; issue: when we parse a whitespace or a newline and got an #eof accidentally
               ; we should not return an error "unparsed sentence" but just an empty list
               ; todo: do the same as issue fix in "lang/eval"
               (values l r p val))))

      (define get-sexps
         (greedy* sexp-parser))

      (define get-padded-sexps
         (let-parse*(
               (data get-sexps)
               (skip maybe-whitespace))
            data))


      ;; ;; fixme: new error message info ignored, and this is used for loading causing the associated issue
      ;; (define (read-exps-from data done fail)
      ;;    (let*/cc ret  ;; <- not needed if fail is already a cont
      ;;       ((data
      ;;          (utf8-decoder data
      ;;             (λ (self line data)
      ;;                (ret (fail (list "Bad UTF-8 data on line " line ": " (ltake line 10))))))))
      ;;       (sexp-parser data
      ;;          (λ (data drop val pos)
      ;;             (cond
      ;;                ((eof? val) (reverse done))
      ;;                ((null? data) (reverse (cons val done))) ;; only for non-files
      ;;                (else (read-exps-from data (cons val done) fail))))
      ;;          (λ (pos reason)
      ;;             (if (null? done)
      ;;                (fail "syntax error in first expression")
      ;;                (fail (list 'syntax 'error 'after (car done) 'at pos))))
      ;;          0)))

      (define (list->number lst base)
         (parse (number-in-base base) lst #false #false #false))

      ;; (define (string->sexp str fail errmsg)
      ;;    (parse (sexp) (str-iter str) #false errmsg fail))

      ;; parse all contents of vector to a list of sexps, or fail with
      ;; fail-val and print error message with further info if errmsg
      ;; is non-false

      ;; (define (bytevector->sexps vec fail errmsg)
      ;;    ; parse parser data maybe-path maybe-error-msg fail-val
      ;;    (let ((lst (vector->list vec)))
      ;;       (parse get-sexps lst #false errmsg fail)))

      (define (list->sexps lst fail errmsg)
         ; parse parser data maybe-path maybe-error-msg fail-val
         (parse get-padded-sexps lst #false errmsg fail))

      ; backward compatibility
      (define get-number number)

      ; few sexp stream functions
      (define (parser-succ l r p v)
         (values l r p v))

      (define (bytestream->exp-stream ll parser fail)
         (λ ()
            (let*((lp r p val (parser #null ll 0 parser-succ)))
               (cond
                  (lp ;; something parsed successfully
                     (lcons val (bytestream->exp-stream r parser fail)))
                  ((null? r) ;; end of input
                     #null)
                  ((function? fail) ; (fail cont r reason)
                     (fail
                        (λ (ll) (bytestream->exp-stream ll parser fail))
                           r val))
                  (else
                     #null)))))

      (define (fd->exp-stream fd parser fail) ; todo: remove prompt
         (bytestream->exp-stream (port->bytestream fd) parser fail))

      (define (file->exp-stream path parser fail)
         (let ((fd (open-input-file path)))
            (if fd
               (fd->exp-stream fd parser fail))))

))
