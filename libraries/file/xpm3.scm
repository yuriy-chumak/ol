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
               ( -- (byte #\*))
               ( -- (byte #\/)))
            'comment)
         (let-parse* (
               ( -- byte)
               ( -- (block-comment)))
            'comment)))

   (define whitespace
      (either
         (byte (lambda (c) (has? '(#\tab #\newline #\space #\return) c)))
         (let-parse* (
               ( -- (byte #\/))
               ( -- (byte #\*))
               ( -- (block-comment)))
            'comment)))

   (define maybe-whitespaces (greedy* whitespace))

   (define rest-of-line
      (let-parse*
         ((chars (greedy* (byte (lambda (x) (not (eq? x #\newline))))))
            ( -- (byte #\newline)))
         chars))

   (define (digit? x) (<= #\0 x #\9))

   (define xpm3-parser
      (let-parse* (
            ; header:
            ( -- maybe-whitespaces)
            ( -- (bytes "static char *"))
            ( -- rest-of-line)
            ( -- maybe-whitespaces)
            ; parameters:

            ( -- (byte #\"))
            (width number)
            ( -- (byte #\space))
            (height number)
            ( -- (byte #\space))
            (colors number)
            ( -- (byte #\space))
            (bpp (bytes "1 " 1)) ; supported only 1
            ( -- rest-of-line)

            ; colour table:
            (color-table (times colors
               (let-parse* (
                     ( -- (byte #\"))
                     (key byte)
                     ( -- maybe-whitespaces)
                     (ctype byte)
                     ( -- maybe-whitespaces)
                     (color (greedy+ (byte (lambda (b) (not (eq? b #\"))))))
                     ( -- rest-of-line))
                  [key ctype color])))
            ; bitmap
            ( -- maybe-whitespaces)
            (bitmap (times height
               (let-parse* (
                     ( -- (byte #\"))
                     (row (times width byte))
                     ( -- (byte #\"))
                     ( -- rest-of-line))
                  row)))
            ; end
            ( -- (bytes "};"))
            ( -- maybe-whitespaces))
      {
         'width width
         'height height
         'colors colors

         'color-table color-table
         'bitmap (make-vector (map make-vector bitmap))
      }))

   (define (xpm3-parse-file filename)
      (parse xpm3-parser (file->bytestream filename)))
))
