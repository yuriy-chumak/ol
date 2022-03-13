(define-library (lib pango)
   (export
      ; (lib pango layout):
      PangoEllipsizeMode

      PANGO_ELLIPSIZE_NONE
      PANGO_ELLIPSIZE_START
      PANGO_ELLIPSIZE_MIDDLE
      PANGO_ELLIPSIZE_EN
   )
   (import
      (scheme core)
      (otus ffi))

(begin

   (define PangoEllipsizeMode fft-int)

   (define PANGO_ELLIPSIZE_NONE   0)
   (define PANGO_ELLIPSIZE_START  1)
   (define PANGO_ELLIPSIZE_MIDDLE 2)
   (define PANGO_ELLIPSIZE_EN     3)

))