#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

(print "
---------------------------------------------------------------
parameterless function returning numeric type by value (neutral values, type limits)
(such calls are optimized for execution, no stack and heap sizes check for params)
type cN_()
{
   type y = X;
   printf('{{ () => format }}', y); fflush(stdout);
   return y;
}")
(for-each (lambda (index typename Nn)
      (for-each (lambda (n)
            (define name (string-append index (s/-/m/ (number->string n)) "_"))
            (define function (this typename name))

            (try name function '()))
         Nn))
   ; unsigned types                                                                  ; signed types                                                                                                             ; floating points
   '("C"                "S"                 "I"                "L"                     "c"                          "s"                            "i"                            "l"                             "f"                 "d"                )
   `(,fft-unsigned-char ,fft-unsigned-short ,fft-unsigned-int  ,fft-unsigned-long-long ,fft-signed-char             ,fft-signed-short              ,fft-signed-int                ,fft-signed-long-long           ,fft-float          ,fft-double        )
   `((0 1 ,UINT8_MAX)   (0 1 ,UINT16_MAX)   (0 1 ,UINT32_MAX)  (0 1 ,UINT64_MAX)       (,INT8_MIN -1 0 1 ,INT8_MAX) (,INT16_MIN -1 0 1 ,INT16_MAX) (,INT32_MIN -1 0 1 ,INT32_MAX) (,INT64_MIN -1 0 1 ,INT64_MAX)  (-1e10 -1 0 1 1e10) (-1e48 -1 0 1 1e48))
)
