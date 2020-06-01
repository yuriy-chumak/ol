(define-library (lib cairo)
   (export
      cairo_t*

      cairo_set_source_rgb
      cairo_set_line_width
      cairo_move_to
      cairo_line_to

      cairo_stroke cairo_fill
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin

(define cairo_t* fft-void*)

(define CAIRO (load-dynamic-library "libcairo.so"))

(define cairo_set_source_rgb (CAIRO fft-void "cairo_set_source_rgb" cairo_t* fft-double fft-double fft-double))
(define cairo_set_line_width (CAIRO fft-void "cairo_set_line_width" cairo_t* fft-double))

(define cairo_move_to (CAIRO fft-void "cairo_move_to" cairo_t* fft-double fft-double))
(define cairo_line_to (CAIRO fft-void "cairo_line_to" cairo_t* fft-double fft-double))

(define cairo_stroke (CAIRO fft-void "cairo_stroke" cairo_t*))
(define cairo_fill (CAIRO fft-void "cairo_fill" cairo_t*))
))