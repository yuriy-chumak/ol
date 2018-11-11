(define-library (file xml)
(import
   (otus lisp)
   (only (lang intern) string->symbol)
   (owl parse))

(export
   xml-parser)

(begin
   ; utils:
   (define (between? lo x hi) ; fast version of (<= lo x hi))
      (and (or (less? lo x) (eq? lo x))
         (or (less? x hi) (eq? x hi))))
   (define (character? n)
      (or
         (between? #\a n #\z)
         (between? #\A n #\Z)
         (> n 127)))         ;; allow high code points in symbols
   (define (whitespace? x)
      (has? '(#\tab #\newline #\space #\return) x))

   ; ...
   (define skip-whitespaces
      (get-any-of (get-greedy* (get-rune-if whitespace?))))

   (define get-attribute
      (let-parses(
            (name (get-greedy+ (get-rune-if character?)))
            (= (get-imm #\=))
            (* (get-imm #\"))
            (value (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\"))))))
            (* (get-imm #\"))
            (* skip-whitespaces))
         (cons
            (string->symbol (runes->string name))
            (runes->string value))))

   (define (make-xmltag-parser)
      (let-parses(
            (< (get-imm #\<))
            (tag (get-greedy+ (get-rune-if character?)))
            (* skip-whitespaces)
            (attributes (get-greedy* get-attribute))
            (body (get-either
                        (get-word "/>" #null)
                        (let-parses(
                              (* (get-imm #\>))
                              (* skip-whitespaces)
                              (innerText (get-greedy* (get-rune-if (lambda (x) (not (eq? x #\<))))))
                              (innerXml (get-greedy* (make-xmltag-parser)))
                              (* (get-word "</" #t)) ; </tag>
                              (* (get-word (runes->string tag) #t))
                              (* (get-imm #\>))
                              (* skip-whitespaces))
                           (if (null? innerXml) (runes->string innerText) innerXml))))
            (* skip-whitespaces))
         (tuple
            (string->symbol (runes->string tag))
            (list->ff attributes)
            body)))


   (define xml-parser
      (let-parses(
            ; <?xml version="1.0" encoding="UTF-8"?>
            (* (get-word "<?xml" #true))
            (* (get-greedy+ (get-byte-if (λ (x) (not (eq? x #\?))))))
            (* (get-word "?>\n" #false))

            ; <map ...
            (body (make-xmltag-parser)))
         body))

))
