(define-library (file ini)
(import (otus lisp)
      (owl parse))
(export
   ini-parse-file)
(begin

   (define (whitespace? x)
      (has? '(#\tab #\space #\newline #\return) x))

   (define skip-whitespaces
      (get-greedy* (get-rune-if whitespace?)))

   (define get-rest-of-line
      (let-parses
         ((chars (get-greedy* (get-byte-if (lambda (x) (not (has? '( #\newline #\return) x))))))
         (skip  (get-greedy+ (get-byte-if (lambda (x) (has? '( #\newline #\return) x))))))
         chars))

   (define (between? lo x hi) ; fast version of (<= lo x hi)), where x is rune
      (and (or (less? lo x) (eq? lo x))
         (or (less? x hi) (eq? x hi))))

   (define (character? n) (or
      (between? #\a n #\z)
      (between? #\0 n #\9)
      (between? #\A n #\Z)))
      ;(between? #x7F n #xD7FF)
      ;(between? #xE000 n #xFFFD)
      ;(between? #x10000 n #x10FFFF)))

   (define get-keyvalue
      (let-parses (
            (key (get-greedy+ (get-rune-if character?)))
            (* (get-imm #\=))
            (value get-rest-of-line)
            (* skip-whitespaces))
         (cons
            (string->symbol (bytes->string key))
            (bytes->string value))))


   (define get-section
      (let-parses (
            (* (get-imm #\[))
            (name (get-greedy+ (get-rune-if character?)))
            (* (get-imm #\]))
            (* skip-whitespaces)
            (pairs (get-greedy* get-keyvalue))
            (* skip-whitespaces))
         (cons
            (string->symbol (bytes->string name))
            (pairs->ff pairs))))

   (define ini-parser
      (let-parses (
            (sections (get-greedy+ get-section))
            (* skip-whitespaces))
         sections))

   (define (ini-parse-file filename)
      (let ((file (open-input-file filename)))
         (if file
            (let ((o (parse ini-parser (port->bytestream file) filename "ini parse error" #false)))
               (if o o
                  (begin (close-port file) #false)))))) ; no automatic port closing on error
))
