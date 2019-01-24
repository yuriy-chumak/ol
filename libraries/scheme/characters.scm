;;;; 6.3.4  Characters

(define-library (scheme characters)
(import
   (scheme core))

(export
   char?
   char=? char<? char>?
   char<=? char>=?
   char->integer
   integer->char
)

(begin
   ; procedure:  (char? obj)
   (define (char? o) (eq? (type o) type-fix+))

   ; procedure:  (char=? char1 char2)
   (define char=? eq?)

   ; procedure:  (char<? char1 char2)
   (define char<? less?)

   ; procedure:  (char>? char1 char2)
   (define (char>? a b)
      (char<? b a))

   ; procedure:  (char<=? char1 char2)
   (define (char<=? a b)
      (or (eq? a b)
          (less? a b)))

   ; procedure:  (char>=? char1 char2)
   (define (char>=? a b)
      (char<=? b a))

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

   (define (char->integer x) x)
   (define (integer->char x) x)

))
