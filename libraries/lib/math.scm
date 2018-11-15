(define-library (lib math)
   (import (scheme core)
           (otus ffi))
   (export
      sin cos sqrt
)

(begin

   (define libm (or (load-dynamic-library "libm.so.6")
                    (load-dynamic-library "ntdll.dll")
                    (runtime-error "Can't load libm" #f)))

   ; sin / cos
   (define sin (libm fft-double "sin" fft-double))
   (define cos (libm fft-double "cos" fft-double))

   ; sqrt
   (define sqrt (libm fft-double "sqrt" fft-double))
))
