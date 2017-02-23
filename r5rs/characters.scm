;;;; 6.3.4  Characters

(define-library (r5rs characters)
(import
   (r5rs core))

(export
   char?
   char->integer
   integer->char
)

(begin
   ; procedure:  (char? obj)
   (define (char? o) (eq? (type o) type-fix+))
   ; procedure:  (char=? char1 char2)
   ; procedure:  (char<? char1 char2)
   ; procedure:  (char>? char1 char2)
   ; procedure:  (char<=? char1 char2)
   ; procedure:  (char>=? char1 char2)
   ; library procedure:  (char-ci=? char1 char2)
   ; library procedure:  (char-ci<? char1 char2)
   ; library procedure:  (char-ci>? char1 char2)
   ; library procedure:  (char-ci<=? char1 char2)
   ; library procedure:  (char-ci>=? char1 char2)
   ; library procedure:  (char-alphabetic? char)
   ; library procedure:  (char-numeric? char)
   ; library procedure:  (char-whitespace? char)
   ; library procedure:  (char-upper-case? letter)
   ; library procedure:  (char-lower-case? letter)
   ; procedure:  (char->integer char)
   ; procedure:  (integer->char n)
   ; library procedure:  (char-upcase char)
   ; library procedure:  (char-downcase char)

   (define self (λ (x) x))

   (define char->integer self)
   (define integer->char self)

))
