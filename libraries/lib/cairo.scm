(define-library (lib cairo)
   (export
      cairo_t*

      cairo_set_source_rgb
      cairo_set_line_width
      cairo_move_to
      cairo_line_to
      cairo_rectangle

      cairo_stroke
      cairo_stroke_preserve
      cairo_fill
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin

(define cairo_t* fft-void*)
(define void fft-void)
(define double fft-double)

(define CAIRO (load-dynamic-library "libcairo.so"))

(define cairo_set_source_rgb (CAIRO void "cairo_set_source_rgb" cairo_t* fft-double fft-double fft-double))
(define cairo_set_line_width (CAIRO void "cairo_set_line_width" cairo_t* fft-double))

(define cairo_move_to (CAIRO void "cairo_move_to" cairo_t* fft-double fft-double))
(define cairo_line_to (CAIRO void "cairo_line_to" cairo_t* fft-double fft-double))

(define cairo_stroke (CAIRO void "cairo_stroke" cairo_t*))
(define cairo_stroke_preserve (CAIRO void "cairo_stroke_preserve" cairo_t*))
(define cairo_fill (CAIRO void "cairo_fill" cairo_t*))

(define cairo_rectangle (CAIRO void "cairo_rectangle" cairo_t* double double double double))

))