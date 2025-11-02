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
   ; floating points
   '("f"                 "d"                )
   `(,fft-float          ,fft-double        )
   `((-1e10 -1 0 1 1e10) (-1e48 -1 0 1 1e48))
)
