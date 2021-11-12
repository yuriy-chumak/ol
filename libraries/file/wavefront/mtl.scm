; http://paulbourke.net/dataformats/mtl/
(define-library (file wavefront mtl)

(export
   wavefront-mtl-parser)

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

(define get-comment
   (let-parses(
         (sign (get-imm #\#))
         (comment get-rest-of-line))
      #true))

(define get-newmtl
   (let-parses(
         (skip (get-word "newmtl " #t))
         (name get-rest-of-line))
      name))

(define (get-1-number name)
   (let-parses(
         (skip (get-word name #t))
         (value get-number)
         (skip (get-imm #\newline)))
      value))
(define (get-3-numbers name)
   (let-parses(
         (skip (get-word name #t))
         (r get-number)
         (skip (get-imm #\space))
         (g get-number)
         (skip (get-imm #\space))
         (b get-number)
         (skip (get-imm #\newline)))
      [r g b 1.0]))

(define material-parser
   (let-parses(
         (skip (get-imm #\newline))
         (newmtl get-newmtl)
         (ns (get-1-number "Ns "))
         (ka (get-3-numbers "Ka "))
         (kd (get-3-numbers "Kd "))
         (ks (get-3-numbers "Ks "))
         (ke (get-3-numbers "Ke "))
         (ni (get-1-number "Ni "))
         (d (get-1-number "d "))
         (illum (get-1-number "illum ")))
      {
         'name   (bytes->string newmtl)
         'ns     ns
         'ka     ka
         'kd     kd
         'ks     ks
         'ke     ke
         'ni     ni
         'd      d
         'illum  illum
      }))

; main
(define wavefront-mtl-parser
   (let-parses(
         (comments (get-greedy* get-comment))
         (materials (get-greedy+ material-parser)))
      materials))

))
