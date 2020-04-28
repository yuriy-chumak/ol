;;;
;;; owl cfg parsing combinators and macros
;;;

(define-library (owl parse)

   (export
      let-parses ; parser constructor
      let-parse* ; same as let-parses

      get-byte   ; elementary parsers
      get-byte-if
      get-epsilon
      get-rune
      get-rune-if
      get-imm
      get-word
      get-word-ci       ; placeholder
      get-one-of
      get-either
      get-any-of
      get-kleene*
      get-kleene+
      get-greedy*
      get-greedy+

      parse try-parse         ; full stream and partial stream parsers. x ll x path|#false x errmsg|#false x fail-val
      peek)

   (import
      (scheme base)
      (scheme char)

      (owl lazy)
      (owl math)
      (owl list)
      (owl string)
      (owl list-extra)
      (owl unicode)
      (owl io)
      (owl render)
      (owl interop))

   (begin

      ; parser format:
      ; (parser ll ok fail pos)
      ;      -> (ok ll' fail' val pos)
      ;      -> (fail fail-pos fail-msg')

      (define-syntax let-parse*
         (syntax-rules (verify eval)
            ((let-parse* 42 sc ft lst pos ((val (eval term)) . r) body)
               (let ((val term))
                  (let-parse* 42 sc ft lst pos r body)))
            ((let-parse* 42 sc ft lst pos ((val parser) . r) body)
               (parser lst
                  (λ (lst ft val pos)
                     (let-parse* 42 sc ft lst pos r body))
                  ft pos))
            ((let-parse* 42 sc ft lst pos () body) (sc lst ft body pos))
            ((let-parse* 42 sc ft lst pos ((verify term msg) . r) body)
               (if term
                  (let-parse* 42 sc ft lst pos r body)
                  (ft pos msg))) ; bug: not showing error message
            ((let-parse* ((a . b) ...) body)
               (λ (ll ok fail pos)
                  (let-parse* 42 ok fail ll pos ((a . b) ...) body)))))

      (define-syntax let-parses
         (syntax-rules ()
            ((let-parses . body)
               (let-parse* . body))))

      ; read nothing, succeed with val
      (define (get-epsilon val)
         (λ (ll ok fail pos)
            (ok ll fail val pos)))

      ; read a byte
      (define (get-byte ll ok fail pos)
         (cond
            ((null? ll) (fail pos "end of input")) ; will always be the largest value
            ((pair? ll) (ok (cdr ll) fail (car ll) (+ pos 1)))
            (else (get-byte (ll) ok fail pos))))

      ; read a byte by predicate
      (define (get-byte-if pred)
         (let-parses (
               (b get-byte)
               (verify (pred b) `(bad byte ,b)))
            b))

      ;; (define peek-mark "loltron")
      ;; (define (peek-val? x) (if (pair? x) (eq? (car x) peek-mark) #false))

      ; make sure the next thing is *not* accepted by parser (unfortunate name, change later)
      (define (peek parser) ; fixme, add error message
         (λ (lst ok fail pos)
            (parser lst
               (λ (lst fail val pos)
                  ; we do *not* want a match
                  (fail lst "peek matched"))
               (λ (fpos fmsg)
                  ; ok not to match
                  (ok lst fail 'peeked pos))
               pos)))

      (define (get-imm n)
         (let-parses (
               (a get-byte)
               (verify (eq? a n) `(expected ,n)))
            a))

      (define (get-word str val)
         (let ((bytes (string->runes str)))
            (λ (lst ok fail pos)
               (let loop ((bytes bytes) (lst lst) (fail fail) (pos pos))
                  (if (null? bytes)
                     (ok lst fail val pos)
                     (get-byte lst
                        (λ (lst fail byte pos)
                           (if (eq? byte (car bytes))
                              (loop (cdr bytes) lst fail pos)
                              (fail pos (list "expected next '" (runes->string bytes) "'"))))
                        fail pos))))))

      ;; fixme: not correct yet
      (define (get-word-ci str val)
         (let ((bytes (string->runes str)))
            (λ (lst ok fail pos)
               (let loop ((bytes bytes) (lst lst) (fail fail) (pos pos))
                  (if (null? bytes)
                     (ok lst fail val pos)
                     (get-byte lst
                        (λ (lst fail byte pos)
                           (if (char-ci=? byte (car bytes))
                              (loop (cdr bytes) lst fail pos)
                              (fail pos (list "expected next '" (runes->string bytes) "'"))))
                        fail pos))))))

      (define (get-one-of bytes)
         (get-byte-if (λ (x) (has? bytes x))))

      (define (get-either a b)
         (λ (lst ok fail pos)
            (a lst ok
               (λ (fa fai)
                  (b lst ok (λ (fb fbi) (if (< fa fb) (fail fb fbi) (fail fa fai))) pos))
               pos)))

      (define (get-kleene* par)
         (get-either
            (let-parses
               ((hd par) (tl (get-kleene* par)))
               (cons hd tl))
            (get-epsilon null)))

      ; get all of successful parses of parser not allowing backtracking
      ; intention being that "aaaaa" has quite a few combinations of (kleene+ a),
      ; and when describing something like lexical structure, which is usually
      ; handled by a pass greedily matching regular expressions, this may cause
      ; unexpected exponential slowdowns on parse errors when using simple
      ; parsing combinators like these in lib-parse.

      (define (get-greedy parser zero-ok?)
         (λ (lst ok fail pos)
            (let loop ((lst lst) (rvals null) (pos pos))
               (parser lst
                  (λ (lst fail val pos)
                     (loop lst (cons val rvals) pos))
                  (λ (fpos freason)
                     (if (or zero-ok? (pair? rvals))
                        (ok lst fail (reverse rvals) pos) ; pass the original failure cont
                        (fail fpos freason))) ; could fail differently when zero and requested at least one
                  pos))))

      (define (get-greedy* parser) (get-greedy parser #true))
      (define (get-greedy+ parser) (get-greedy parser #false))

      (define (get-kleene+ what)
         (let-parses
            ((hd what)
             (tl (get-kleene* what)))
            (cons hd tl)))

      (define-syntax get-any-of
         (syntax-rules ()
            ((get-any-of a) a)
            ((get-any-of a b) (get-either a b))
            ((get-any-of a . bs)
               (get-either a (get-any-of . bs)))))

      (define (get-between below above)
         (get-byte-if
            (λ (x)
               (and (less? below x) (less? x above)))))

      ; #b10xxxxxx
      (define get-extension-byte
         (let-parse* (
               (b get-byte)
               (verify (eq? (vm:and b #b11000000) #b10000000) "Bad extension byte"))
            b))

      ;; fixme: get-rune == get-utf-8
      (define get-rune
         (get-any-of
            (get-byte-if (λ (x) (less? x 128)))
            (let-parse* (
                  (a (get-between 127 224))
                  ; note: two byte sequence #b11000000 10xxxxxx is invalid
                  (verify (not (eq? a #b11000000)) "invalid utf-8 stream")
                  (b get-extension-byte))
               (two-byte-point a b))
            (let-parse* (
                  (a (get-between 223 240))
                  ; note: three byte sequence #b11100000 10xxxxxx 10xxxxxx is valid!
                  (b get-extension-byte) (c get-extension-byte))
               (three-byte-point a b c))
            (let-parse* (
                  (a (get-between 239 280))
                  ; note: four byte sequence #b11110000 10xxxxxx 10xxxxxx 10xxxxxx is valid!
                  (b get-extension-byte) (c get-extension-byte) (d get-extension-byte))
               (four-byte-point a b c d))))

      (define (get-rune-if pred)
         (let-parse* (
               (rune get-rune)
               (verify (pred rune) "invalid rune"))
            rune))


      ;;;
      ;;; Port data streaming and parsing
      ;;;

      ; this is fairly distinct from the rest of lib-parse, because it mainly deals with
      ; IO operation sequencing.

      ; notice that this difficulty comes from owl not havign side-effects on data structures
      ; even in the VM level, ruling out lazy lists and and manually mutated streams, which
      ; are usually used in functional parsers.

      (define (stdio-port? port)
         (let ((stdioports (list stdin stdout stderr)))
            (has? stdioports port)))

      (define (print-syntax-error reason bytes posn)
         (print-to stderr reason)
         (write-bytes stderr '(#\space #\space #\space)) ; indent by 3 spaces
         (write-bytes stderr (cons #\' (append (force-ll bytes) '(#\' #\newline))))
         (write-bytes stderr (repeat #\space (+ posn 4))) ; move to right position
         (write-bytes stderr '(#\^ #\newline)))

      ; find the row where the error occurs
      ; keep the row number stored so it can be shown in output
      (define (print-row-syntax-error path reason bytes err-posn)
         (let row-loop ((row 1) (bytes bytes) (pos 0) (rthis null))
            (cond
               ((null? bytes)
                  (for-each (λ (x) (display-to stderr x)) (list path ":" row " "))
                  (print-syntax-error reason (reverse rthis) (- pos err-posn)))
               ((not (pair? bytes)) ; force
                  (row-loop row (bytes) pos rthis))
               ((= (car bytes) 10)
                  (if (> err-posn pos)
                     (row-loop (+ row 1) (cdr bytes) (+ pos 1) null)
                     (begin
                        (for-each display (list path ":" row " "))
                        (print-syntax-error reason (reverse rthis) (- (length rthis) (+ 1 (- pos err-posn)))))))
               (else
                  (row-loop row (cdr bytes) (+ pos 1) (cons (car bytes) rthis))))))

      (define (has-newline? ll)
         (cond
            ((null? ll) #false)
            ((not (pair? ll)) (has-newline? (ll)))
            ((eq? (car ll) 10) #true)
            (else (has-newline? (cdr ll)))))

      ; can be unforced
      (define (null-ll? ll)
         (cond
            ((null? ll) #true)
            ((pair? ll) #false)
            (else (null-ll? (ll)))))


      ; try to parse all of data with given parser, or return fail-val
      ; printing a nice error message if maybe-error-msg is given
      (define (parse parser data maybe-path maybe-error-msg fail-val)
         (parser data
            (λ (data fail val pos)
               (if (null-ll? data)
                  ; all successfully parsed
                  val
                  ; something unparsed. backtrack.
                  (fail pos "out of data")))
            (λ (pos reason)
               ; print error if maybe-error-msg is given
               (if maybe-error-msg
                  (if (or maybe-path (has-newline? data))
                     (print-row-syntax-error
                        (or maybe-path "input")
                        (if (eq? maybe-error-msg #true) reason maybe-error-msg) data pos)
                     (print-syntax-error (if (eq? maybe-error-msg #true) reason maybe-error-msg) data (- pos 1)))) ; is the one from preceding newlines?
               fail-val)
            0))

      ; returns pair (parsed-value . rest-of-stream) or #false if error
      (define (try-parse parser data show-error)
         (parser data
            (λ (data fail val pos)
               (cons val data))
            (λ (pos reason)
               ; print error if maybe-error-msg is given
               (if show-error
                  (print-syntax-error reason data (- pos 1)))
               #false)
            0))
))
