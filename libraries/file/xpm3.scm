(define-library (file xpm3)
(import
   (otus lisp)
   (file parser)
   (data s-exp))

(export
   xpm3-parser
   xpm3-parse-file)

; parsed xpm3 as:
;   vector of vectors of colors

(begin
   (define (block-comment)
      (either
         (let-parse* (
               (skip (get-imm #\*))
               (skip (get-imm #\/)))
            'comment)
         (let-parse* (
               (skip byte)
               (skip (block-comment)))
            'comment)))

   (define whitespace
      (either
         (byte-if (lambda (c) (has? '(#\tab #\newline #\space #\return) c)))
         (let-parse* (
               (skip (get-imm #\/))
               (skip (get-imm #\*))
               (skip (block-comment)))
            'comment)))

   (define maybe-whitespaces (greedy* whitespace))

   (define rest-of-line
      (let-parse*
         ((chars (greedy* (byte-if (lambda (x) (not (eq? x #\newline))))))
            (skip (imm #\newline)))
         chars))

   (define (digit? x) (<= #\0 x #\9))

   (define xpm3-parser
      (let-parse* (
            ; header:
            (? maybe-whitespaces)
            (? (get-word "static char *" #t))
            (? rest-of-line)
            (? maybe-whitespaces)
            ; parameters:

            (? (imm #\"))
            (width number)
            (? (imm #\space))
            (height number)
            (? (imm #\space))
            (colors number)
            (? (imm #\space))
            (bpp (word "1 " 1)) ; supported only 1
            (? rest-of-line)

            ; colour table:
            (color-table (times colors
               (let-parse* (
                     (? (imm #\"))
                     (key byte)
                     (? maybe-whitespaces)
                     (ctype byte)
                     (? maybe-whitespaces)
                     (color (greedy+ (byte-if (lambda (b) (not (eq? b #\"))))))
                     (? rest-of-line))
                  [key ctype color])))
            ; bitmap
            (? maybe-whitespaces)
            (bitmap (times height
               (let-parse* (
                     (? (imm #\"))
                     (row (times width byte))
                     (? (imm #\"))
                     (? rest-of-line))
                  row)))
            ; end
            (? (word "};" #t))
            (? maybe-whitespaces))
      {
         'width width
         'height height
         'colors colors

         'color-table color-table
         'bitmap (make-vector (map make-vector bitmap))
      }))

   (define (xpm3-parse-file filename)
      (parse xpm3-parser (file->bytestream filename) filename "xpm3 parse error" #f))
))
