#!/usr/bin/env -S ../../ffi
,load "definitions"

(define (try tag function . numbers)
   (for-each display (list "   " tag " " numbers ">"))
   (let ((out (apply function numbers)))
      (print " = " out)))

(print "
// too much arguments (4 needed, more provided):
long long function(char c, short s, int i, long long q)
{
   return c+s+i+q;
}")
(let ((function (this fft-long-long "csiq2q" fft-char fft-short fft-int fft-long-long)))
   (try "16 arguments" function
      1 2 3 4 5 6 7 8
      1 2 3 4 5 6 7 8)
   (try "64 arguments" function
      1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8
      1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8))
