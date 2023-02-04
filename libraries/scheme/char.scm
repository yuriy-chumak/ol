(define-library (scheme char)
   (export
      digit-value

      char-ci=?
      char-ci<?
      char-ci>?
      char-ci<=?
      char-ci>=?

      char-upcase
   ;;    char-downcase
   ;;    char-foldcase

      char-alphabetic?
      char-numeric?
      char-whitespace?
   ;;    char-upper-case?
   ;;    char-lower-case?

   ;;    digit-value
   ;;    string-downcase
   ;;    string-foldcase
   ;;    string-upcase

      string-ci<=?       ; str str → bool
      string-ci<?        ; str str → bool
      string-ci=?        ; str str → bool
      string-ci>?        ; str str → bool
      string-ci>=?       ; str str → bool

   )

   (import
      (scheme base)
      (owl list)
      (srfi 1)
      (owl lazy)
      (owl math)
      (owl string)
      (owl ff) (owl iff))
   (include "owl/unicode-char-folds.scm")
   (include "scheme/unicode/alphabetic-chars.scm")
   (include "scheme/unicode/numeric-chars.scm")
   (include "scheme/unicode/whitespace-chars.scm")

   (begin

      (define (left a b) a)
      (define (putT a) (cons a #T))

      ; unicode symbols
      (define alphabetic-chars (alist->ff alphabetic-chars-alist))
      (define numeric-chars (alist->ff numeric-chars-alist))
      (define whitespace-chars (alist->ff whitespace-chars-alist))


      (define (char-alphabetic? ch)
         (alphabetic-chars ch #false)) ; any unicode char is an alphabetic

      (define (char-numeric? ch)
         (numeric-chars ch #false))

      (define (char-whitespace? ch)
         (whitespace-chars ch #false))

      (define (digit-value ch)
         (let ((digit (numeric-chars ch #f)))
            (if (char? digit)
               digit)))

      ; * internal staff
      ; large table 'char => uppercase char'
      (define char-fold-iff
         (fold
            (λ (iff node)
               (if (eq? (length node) 2)
                  (iput iff (car node) (cadr node))
                  (iput iff (car node) (cdr node))))
            #empty char-folds))

      (define (compare cmp a b)
         (let loop ((a a) (b b))
            (or (null? b)
                (and (cmp a (car b))
                     (loop (car b) (cdr b))))))

      ; procedure:  (char-ci=? char1 char2 ...)
      (define (=? a b)
         (or (eq? a b)
             (eq? (iget char-fold-iff a a)
                  (iget char-fold-iff b b))))

      (define (char-ci=? a . b)
         (compare =? a b))

      (assert (char-ci=? #\a)           ===> #true)
      (assert (char-ci=? #\a #\A)       ===> #true)
      (assert (char-ci=? #\A #\A #\a)   ===> #true)
      (assert (char-ci=? #\a #\b)       ===> #false)
      (assert (char-ci=? #\Σ #\σ)       ===> #true) ; greek 'sigma'
      (assert (char-ci=? #\я #\Я)       ===> #true) ; cyrillic 'ja'
      (assert (char-ci=? #\ä #\Ä)       ===> #true) ; baltic 'aeae'


      ; procedure:  (char-ci<? char1 char2 ...)
      (define (<? a b)
          (less? (iget char-fold-iff a a)
                 (iget char-fold-iff b b)))

      (define (char-ci<? a . b)
         (compare <? a b))

      (assert (char-ci<? #\a)           ===> #true)
      (assert (char-ci<? #\a #\B)       ===> #true)
      (assert (char-ci<? #\b #\A)       ===> #false)
      (assert (char-ci<? #\A #\b #\C)   ===> #true)
      (assert (char-ci<? #\A #\c #\b)   ===> #false)
      (assert (char-ci<? #\a #\a)       ===> #false)
      (assert (char-ci<? #\у #\Я)       ===> #true) ; cyrillic
      (assert (char-ci<? #\У #\я)       ===> #true)
      (assert (char-ci<? #\Я #\у)       ===> #false)
      (assert (char-ci<? #\я #\У)       ===> #false)
      (assert (char-ci<? #\ä #\Ö)       ===> #true) ; baltic

      ; procedure:  (char-ci>? char1 char2 ...)
      (define (>? a b)
          (less? (iget char-fold-iff b b)
                 (iget char-fold-iff a a)))

      (define (char-ci>? a . b)
         (compare >? a b))

      ; procedure:  (char-ci<=? char1 char2 ...)
      (define (<=? a b)
          (or (eq? a b)
              (let ((a (iget char-fold-iff a a))
                    (b (iget char-fold-iff b b)))
                 (or (eq? a b)
                     (less? a b)))))

      (define (char-ci<=? a . b)
         (compare <=? a b))

      ; procedure:  (char-ci>=? char1 char2 ...)
      (define (>=? a b)
          (or (eq? a b)
              (let ((a (iget char-fold-iff a a))
                    (b (iget char-fold-iff b b)))
                 (or (eq? a b)
                     (less? b a)))))

      (define (char-ci>=? a . b)
         (compare >=? a b))

      ; procedure:  (char-upcase char)
      (define (char-upcase char)
         (iget char-fold-iff char char))

      ; ci
      (define (upcase ll)
         (let*((cp ll (uncons ll #false)))
            (if cp
               (let ((cp (char-upcase cp)))
                  (if (pair? cp)
                     (append cp (upcase ll))
                     (lcons cp (upcase ll))))
               null)))

      ; ---------------------------------------------------------------
      ; string-ci*

      (define (string-ci<=? a b) (not (eq? 3 (str-compare upcase a b))))
      (define (string-ci<? a b)       (eq? 1 (str-compare upcase a b)))
      (define (string-ci=? a b)       (eq? 2 (str-compare upcase a b)))
      (define (string-ci>? a b)       (eq? 3 (str-compare upcase a b)))
      (define (string-ci>=? a b) (not (eq? 1 (str-compare upcase a b))))

      ; tests
      (assert (string-ci=? "abc" "aBc")   ===> #true)
      (assert (string-ci=? "abc" "cBa")   ===> #false)

))
