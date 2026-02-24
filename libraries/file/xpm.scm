(define-library (file xpm)
(import
   (otus lisp)
   (only (data s-exp) list->number)
   (file parser))

(export
   xpm-parser
   xpm-parse-file)

; parsed xpm is:
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
         ((chars (greedy* (byte (lambda (x) (not (eq? x 10))))))
            (skip (byte 10))) ;; <- note that this won't match if line ends to eof
         chars))

   (define (digit? x) (<= #\0 x #\9))

   (define xpm-parser
      (let-parse* (
            ; header:
            ( -- maybe-whitespaces)
            ( -- (bytes "static char *"))
            ( -- rest-of-line)
            ; parameters:
            ( -- maybe-whitespaces)
            ( -- (byte #\"))
            (width (greedy+ (byte digit?)))
            ( -- (byte #\space))
            (height (greedy+ (byte digit?)))
            ( -- (byte #\space))
            (colors (greedy+ (byte digit?)))
            ( -- (byte #\space))
            (bpp (byte digit?)) ; supported only 1
            ( -- (bytes [#\" #\,]))
            ; color table
            (color-table (times (list->number colors 10)
               (let-parse* (
                     ( -- maybe-whitespaces)
                     ( -- (byte #\"))
                     (key byte)
                     ( -- (byte #\tab))
                     (ctype byte)
                     ( -- (byte #\space))
                     (color (greedy+ (byte (lambda (b) (not (eq? b #\"))))))
                     ( -- (bytes [#\" #\,])))
                  [key ctype color])))
            ; bitmap
            (bitmap (times (list->number height 10)
               (let-parse* (
                     ( -- maybe-whitespaces)
                     ( -- (byte #\"))
                     (row (times (list->number width 10) byte))
                     ( -- (byte #\"))
                     ( -- rest-of-line))
                  row)))

            ;; (out get-byte)
            (/ maybe-whitespaces)
         )
      {
         'width (list->number width 10)
         'height (list->number height 10)
         'colors (list->number colors 10)
         'bpp (list->number (list bpp) 10)
         'color-table color-table
         'bitmap bitmap
      }))

   (define (xpm-parse-file filename)
      (let ((file (open-input-file filename)))
         (if file
            (let ((o (parse xpm-parser (port->bytestream file) filename "xpm parse error" #false)))
               (close-port file)
               o))))
))
