(define-library (file wavefront obj)

(export
   wavefront-obj-parser)

(import
   (otus lisp)
   (file parser)
   (lang sexp))
(begin

(define get-rest-of-line
   (let-parses
      ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x 10))))))
         (skip (get-imm 10))) ;; <- note that this won't match if line ends to eof
      chars))

(define get-inexact
   (let-parse* (
         (number get-number))
      (inexact number)))

(define get-integer
   (let-parse* (
         (numbers (get-greedy+ (get-byte-if (lambda (x) (<= #\0 x #\9))))))
      (list->number numbers 10)))

(define get-comment
   (let-parses(
         (sign (get-imm #\#))
         (comment get-rest-of-line))
      #true))
(define get-mtllib
   (either
      (let-parses(
            (skip (get-word "mtllib " #t))
            (name get-rest-of-line))
         (bytes->string name))
      (epsilon #false)))
(define get-usemtl
   (either
      (let-parses(
            (skip (get-word "usemtl " #t))
            (name get-rest-of-line))
         (bytes->string name))
      (epsilon #false)))

(define get-g
   (let-parses(
         (skip (get-word "g " #t))
         (name get-rest-of-line))
      name))
(define get-o
   (let-parses(
         (skip (get-word "o " #t))
         (name get-rest-of-line))
      name))
(define get-s
   (let-parses(
         (skip (get-word "s " #t))
         (group get-rest-of-line))
      group))
(define get-l
   (let-parses(
         (skip (get-word "l " #t))
         (group get-rest-of-line))
      group))
(define get-v
   (let-parses(
         (skip (get-word "v " #t))
         (x get-inexact)
         (skip (get-imm #\space))
         (y get-inexact)
         (skip (get-imm #\space))
         (z get-inexact)
         (skip (get-imm #\newline)))
      [x y z]))
(define get-vt
   (let-parses(
         (skip (get-word "vt " #t))
         (x get-inexact)
         (skip (get-imm #\space))
         (y get-inexact)
         (skip (get-imm #\newline)))
      [x y]))
(define get-vn
   (let-parses(
         (skip (get-word "vn " #t))
         (x get-inexact)
         (skip (get-imm #\space))
         (y get-inexact)
         (skip (get-imm #\space))
         (z get-inexact)
         (skip (get-imm #\newline)))
      [x y z]))
(define get-v-vt-vn
   (let-parses(
         (a (either get-integer (epsilon #f)))
         (skip (either (get-imm #\/) (epsilon #false)))
         (b (either get-integer (epsilon #f)))
         (skip (either (get-imm #\/) (epsilon #false)))
         (c (either get-integer (epsilon #f))) )
      [a b c]))

(define get-f
   (let-parses(
         (skip (get-word "f " #t))
         (a get-v-vt-vn)
         (skip (get-imm #\space))
         (b get-v-vt-vn)
         (skip (get-imm #\space))
         (c get-v-vt-vn)
         (skip (get-imm #\newline)))
      [a b c]))

(define facegroup-parser
   (let-parses (
         (usemtl get-usemtl)
         (skip (get-greedy* get-s))
         (faces (get-greedy+ get-f))
         (skip (get-greedy* get-l)))
      (cons
         usemtl
         faces)))

; main
(define wavefront-obj-parser
   (let-parses (
         (comments (get-greedy* get-comment))
         (mtllib get-mtllib)
         (objects (greedy+ (let-parses (
               (name (get-either get-g get-o))
               (v (greedy+ get-v))
               (vt (get-greedy* get-vt))
               (vn (get-greedy* get-vn))
               (facegroups (get-greedy+ facegroup-parser))
               )
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

))
