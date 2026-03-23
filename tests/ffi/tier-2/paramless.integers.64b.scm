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
   ; unsigned types          signed types
   '("Q"                     "q"                                   )
   `(,fft-unsigned-long-long ,fft-long-long                        )
   `((0 1 42 ,UINT64_MAX)    (,INT64_MIN -42 -1 0 1 42 ,INT64_MAX) )
)
