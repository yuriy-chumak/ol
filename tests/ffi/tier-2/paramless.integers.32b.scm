#!/usr/bin/env -S ../../ffi
,load "definitions"

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
   ; unsigned types                                                 signed types
   '("C"                 "S"                  "I"                   "c"                                 "s"                                   "i"                                 )
   `(,fft-unsigned-char  ,fft-unsigned-short  ,fft-unsigned-int     ,fft-signed-char                    ,fft-signed-short                     ,fft-signed-int                     )
   `((0 1 42 ,UINT8_MAX) (0 1 42 ,UINT16_MAX) (0 1 42 ,UINT32_MAX)  (,INT8_MIN -42 -1 0 1 42 ,INT8_MAX) (,INT16_MIN -42 -1 0 1 42 ,INT16_MAX) (,INT32_MIN -42 -1 0 1 42 ,INT32_MAX))
)
