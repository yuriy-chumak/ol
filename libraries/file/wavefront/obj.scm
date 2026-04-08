(define-library (file wavefront obj)

(export
   wavefront-obj-parser
   read-wavefront-obj
   read-wavefront-obj-file

   read-wavefront-obj-port
   read-wavefront-obj-stream
)

(import
   (otus lisp)
   (data parse)
   (data s-exp)
   (only (scheme file) call-with-input-file))
(begin

   (define get-rest-of-line
      (let-parse* (
            (chars (greedy* (byte (lambda (x) (not (eq? x 10))))))
            ( -- (byte 10))) ;; <- note that this won't match if line ends to eof
         chars))

   (define get-inexact
      (let-parse* (
            (number get-number))
         (inexact number)))

   (define get-integer
      (let-parse* (
            (numbers (greedy+ (byte (lambda (x) (<= #\0 x #\9))))))
         (list->number numbers 10)))

   (define get-comment
      (let-parse* (
            (sign (byte #\#))
            (comment get-rest-of-line))
         #true))
   (define get-mtllib
      (either
         (let-parse* (
               ( -- (bytes "mtllib "))
               (name get-rest-of-line))
            (bytes->string name))
         (epsilon #false)))
   (define get-usemtl
      (either
         (let-parse* (
               ( -- (bytes "usemtl "))
               (name get-rest-of-line))
            (bytes->string name))
         (epsilon #false)))

   (define get-g
      (let-parse* (
            ( -- (bytes "g "))
            (name get-rest-of-line))
         name))
   (define get-o
      (let-parse* (
            ( -- (bytes "o "))
            (name get-rest-of-line))
         name))
   (define get-s
      (let-parse* (
            ( -- (bytes "s "))
            (group get-rest-of-line))
         group))
   (define get-l
      (let-parse* (
            ( -- (bytes "l "))
            (group get-rest-of-line))
         group))
   (define get-v
      (let-parse* (
            ( -- (bytes "v "))
            (x get-inexact)
            ( -- (byte #\space))
            (y get-inexact)
            ( -- (byte #\space))
            (z get-inexact)
            ( -- (byte #\newline)))
         [x y z]))
   (define get-vt
      (let-parse* (
            ( -- (bytes "vt "))
            (x get-inexact)
            ( -- (byte #\space))
            (y get-inexact)
            ( -- (byte #\newline)))
         [x y]))
   (define get-vn
      (let-parse* (
            ( -- (bytes "vn "))
            (x get-inexact)
            ( -- (byte #\space))
            (y get-inexact)
            ( -- (byte #\space))
            (z get-inexact)
            ( -- (byte #\newline)))
         [x y z]))
   (define get-v-vt-vn
      (let-parse* (
            (a (either get-integer (epsilon #f)))
            ( -- (either (byte #\/) (epsilon #false)))
            (b (either get-integer (epsilon #f)))
            ( -- (either (byte #\/) (epsilon #false)))
            (c (either get-integer (epsilon #f))) )
         [a b c]))

   (define get-f
      (let-parse* (
            ( -- (bytes "f "))
            (a get-v-vt-vn)
            ( -- (byte #\space))
            (b get-v-vt-vn)
            ( -- (byte #\space))
            (c get-v-vt-vn)
            ( -- (byte #\newline)))
         [a b c]))

   (define facegroup-parser
      (let-parse* (
            (usemtl get-usemtl)
            ( -- (greedy* get-s))
            (faces (greedy+ get-f))
            ( -- (greedy* get-l)))
         (cons
            usemtl
            faces)))

   ; main
   (define wavefront-obj-parser
      (let-parse* (
            (comments (greedy* get-comment))
            (mtllib get-mtllib)
            (objects (greedy+ (let-parse* (
                  (name (either get-g get-o))
                  (v (greedy+ get-v))
                  (vt (greedy* get-vt))
                  (vn (greedy* get-vn))
                  (facegroups (greedy+ facegroup-parser)) )
               {
                  'name  (bytes->string name)
                  'v  v
                  'vt vt
                  'vn vn
                  'facegroups facegroups
               }))))
         {
            'mtllib  mtllib
            'v  (foldr append '() (map (lambda (o) (o 'v  '())) objects))
            'vt (foldr append '() (map (lambda (o) (o 'vt '())) objects))
            'vn (foldr append '() (map (lambda (o) (o 'vn '())) objects))
            'o (map (lambda (o) {
                     'name (o 'name)
                     'facegroups (o 'facegroups)
                     })
                  objects)
         }))


   (define (read-wavefront-obj-stream stream)
      (when stream
         (parse wavefront-obj-parser stream)))

   (define (read-wavefront-obj-port port)
      (when port
         (read-wavefront-obj-stream (port->bytestream port))))

   (define read-wavefront-obj (case-lambda
      (() (read-wavefront-obj-port stdin))
      ((source) (cond
         ((port? source) (read-wavefront-obj-port source))
         ((pair? source) (read-wavefront-obj-stream source))))))

   (define (read-wavefront-obj-file filename)
      (call-with-input-file filename (lambda (port)
         (read-wavefront-obj-port port))))

))
