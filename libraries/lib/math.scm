(define-library (lib math)
   (import (scheme core)
           (otus ffi))
   (export
      sin cos
)

(begin

   (define libm (or (load-dynamic-library "libm.so.6")
                    (load-dynamic-library "ntdll.dll")
                    (runtime-error "Can't load libm" #f)))

   (define sin (libm type-inexact "sin" fft-double))
   (define cos (libm type-inexact "cos" fft-double))
))
