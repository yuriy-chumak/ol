; http://paulbourke.net/dataformats/mtl/
(define-library (file wavefront mtl)

(export
   wavefront-mtl-parser
   read-wavefront-mtl
   read-wavefront-mtl-file

   read-wavefront-mtl-port
   read-wavefront-mtl-stream)

(import
   (otus lisp)
   (file parser)
   (data s-exp)
   (only (scheme file) call-with-input-file))

(begin

(define get-rest-of-line
   (let-parse* (
         (chars (greedy* (byte (lambda (x) (not (eq? x 10))))))
         ( --   (byte #\newline))) ;; <- note that this won't match if line ends to eof
      chars))

(define get-inexact
   (let-parse* (
         (number get-number))
      (inexact number)))

(define get-comment
   (let-parse* (
         (sign (byte #\#))
         (comment get-rest-of-line))
      #true))

(define get-newmtl
   (let-parse* (
         ( --  (bytes "newmtl "))
         (name get-rest-of-line))
      name))

(define (get-1-number name)
   (let-parse* (
         ( --  (bytes name))
         (value get-number)
         ( --  (byte #\newline)))
      value))
(define (get-3-numbers name)
   (let-parse* (
         ( --  (bytes name))
         (r get-inexact)
         ( --  (byte #\space))
         (g get-inexact)
         ( --  (byte #\space))
         (b get-inexact)
         ( --  (byte #\newline)))
      [r g b #i1]))

(define (map-parser name)
   (either
      (let-parse* (
            ( --  (bytes name))
            (value get-rest-of-line))
         (bytes->string value))
      (epsilon #false)))

(define material-parser
   (let-parse* (
         ( --  (byte #\newline))
         (newmtl get-newmtl)
         (ns (get-1-number "Ns "))
         (ka (get-3-numbers "Ka "))
         (kd (get-3-numbers "Kd "))
         (ks (get-3-numbers "Ks "))
         (ke (get-3-numbers "Ke "))
         (ni (get-1-number "Ni "))
         (d (get-1-number "d "))
         (illum (get-1-number "illum "))
         (map_kd (map-parser "map_Kd ")))
      {
         'name   (bytes->string newmtl)
         'ns     (inexact ns)
         'ka     ka
         'kd     kd
         'map_kd map_kd
         'ks     ks
         'ke     ke
         'ni     ni
         'd      d
         'illum  illum
      }))


   (define wavefront-mtl-parser
      (let-parse*(
            (comments (greedy* get-comment))
            (materials (greedy+ material-parser)))
         materials))

   (define (read-wavefront-mtl-stream stream)
      (when stream
         (parse wavefront-mtl-parser stream)))

   (define (read-wavefront-mtl-port port)
      (when port
         (read-wavefront-mtl-stream (port->bytestream port))))

   (define read-wavefront-mtl (case-lambda
      (() (read-wavefront-mtl-port stdin))
      ((source) (cond
         ((port? source) (read-wavefront-mtl-port source))
         ((pair? source) (read-wavefront-mtl-stream source))))))

   (define (read-wavefront-mtl-file filename)
      (call-with-input-file filename (lambda (port)
         (read-wavefront-mtl-port port))))

))
