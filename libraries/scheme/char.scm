(define-library (scheme char)
   (export 
   ;;    char-alphabetic?
      char-ci=?
      char-ci<?
      char-ci>?
      char-ci<=?
      char-ci>=?
   ;;    char-downcase
   ;;    char-foldcase
   ;;    char-lower-case?
   ;;    char-numeric?
      char-upcase
   ;;    char-upper-case?
   ;;    char-whitespace?
   ;;    digit-value
   ;;    string-ci<=?
   ;;    string-ci<?
   ;;    string-ci=?
   ;;    string-ci>=?
   ;;    string-ci>?
   ;;    string-downcase
   ;;    string-foldcase
   ;;    string-upcase
   )

   (import
      (scheme core)
      (owl list)
      (owl iff))
   (include "owl/unicode-char-folds.scm")

   (begin

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
      (assert (char-ci=? #\A #\A #\A)   ===> #true)
      (assert (char-ci=? #\a #\b)       ===> #false)
      (assert (char-ci=? #\я #\Я)       ===> #true) ; cyrillic 'ja'
      (assert (char-ci=? #\ä #\Ä)       ===> #true) ; baltic 'ae'


      ; procedure:  (char-ci<? char1 char2 ...)
      (define (<? a b)
          (less? (iget char-fold-iff a a)
                 (iget char-fold-iff b b)))

      (define (char-ci<? a . b)
         (compare <? a b))

      (assert (char-ci<? #\a)           ===> #true)
      (assert (char-ci<? #\a #\B)       ===> #true)
      (assert (char-ci<? #\A #\b #\C)   ===> #true)
      (assert (char-ci<? #\a #\a)       ===> #false)
      (assert (char-ci<? #\а #\Я)       ===> #true) ; cyrillic 'ja'
      (assert (char-ci<? #\ä #\Ö)       ===> #true) ; baltic 'ae'

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

))
