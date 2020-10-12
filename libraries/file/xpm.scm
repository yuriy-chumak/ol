(define-library (file xpm)
(import
   (otus lisp)
   (only (lang intern) string->symbol)
   (only (lang sexp) list->number)
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
         ((chars (greedy* (byte-if (lambda (x) (not (eq? x 10))))))
            (skip (imm 10))) ;; <- note that this won't match if line ends to eof
         chars))

   (define (digit? x) (<= #\0 x #\9))

   (define xpm-parser
      (let-parse* (
            ; header:
            (/ maybe-whitespaces)
            (/ (get-word "static char *" #t))
            (/ rest-of-line)
            ; parameters:
            (/ maybe-whitespaces)
            (/ (imm #\"))
            (width (greedy+ (byte-if digit?)))
            (/ (imm #\space))
            (height (greedy+ (byte-if digit?)))
            (/ (imm #\space))
            (colors (greedy+ (byte-if digit?)))
            (/ (imm #\space))
            (bpp (byte-if digit?)) ; supported only 1
            (/ (imm #\")) (/ (imm #\,))
            ; color table
            (color-table (times (list->number colors 10)
               (let-parse* (
                     (/ maybe-whitespaces)
                     (/ (imm #\"))
                     (key byte)
                     (/ (imm #\tab))
                     (ctype byte)
                     (/ (imm #\space))
                     (color (greedy+ (byte-if (lambda (b) (not (eq? b #\"))))))
                     (/ (imm #\")) (/ (imm #\,)))
                  [key ctype color])))
            ; bitmap
            (bitmap (times (list->number height 10)
               (let-parse* (
                     (/ maybe-whitespaces)
                     (/ (imm #\"))
                     (row (times (list->number width 10) byte))
                     (/ (imm #\"))
                     (/ rest-of-line))
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
