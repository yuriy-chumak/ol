(define-library (lib math)
   (import
      (scheme core)
      (otus ffi) (owl math))
   (export
      exp log sin cos tan
      asin acos atan sqrt)

(cond-expand
   (Linux (begin
      (setq libm (load-dynamic-library "libm.so.6"))))
   (Windows (begin
      (setq libm (load-dynamic-library "ntdll.dll"))))
)

(begin
   (unless libm (runtime-error "Can't load libm" #f))

   (define exp (libm fft-double "exp" fft-double))
   (define log (libm fft-double "log" fft-double))
   (define sin (libm fft-double "sin" fft-double))
   (define cos (libm fft-double "cos" fft-double))
   (define tan (libm fft-double "tan" fft-double))

   ;
   (define asin (libm fft-double "asin" fft-double))
   (define acos (libm fft-double "acos" fft-double))
   (define atan (libm fft-double "atan" fft-double))
   ; procedure: atan y x
   (define _sqrt (libm fft-double "sqrt" fft-double))
   (define (sqrt x)
      (if (< x 0)
         (complex 0 (_sqrt (- x)))
         (_sqrt x)))

   ; procedure: expt z1 z2
   ; procedure: make-rectangular x1 x2
   ; procedure: make-polar x3 x4
   ; procedure: real-part z
   ; procedure: imag-part z
   ; procedure: magnitude z
   ; procedure: angle z
))
