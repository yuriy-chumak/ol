#!/usr/bin/env -S ../../ffi ../../repl
,load "definitions"

(define (try tag function args)
   (for-each display (list "   " (cons tag args) " --> "))
   (let ((out (apply function args)))
      (print " = " out)))

(define F #f)

(print "
---------------------------------------------------------------
all numeric types together")

(define name "v_CSIQcsiqfd")
(define function (this void name uchar ushort uint ullong char short int llong float double))

(try name function (list 0 0 0 0 0 0 0 0 0 0))
(try name function (list 1 1 1 1 1 1 1 1 1 1))
(try name function (list F F F F F F F F F F))
(try name function (list 2 2 2 2 -1 -1 -1 -1 -1 -1))
(try name function (list UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX  INT8_MAX INT16_MAX INT32_MAX INT64_MAX  1e10  1e48))
(try name function (list UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX  INT8_MIN INT16_MIN INT32_MIN INT64_MIN -1e10 -1e48))


; -----------------------------------------------------------------------------
(define name "v_fdfdfdfdfcsiqCSIQi")
(define function (this void name fft-float fft-double fft-float fft-double fft-float fft-double fft-float fft-double fft-float
                                 fft-signed-char fft-signed-short fft-signed-int fft-signed-long-long
                                 fft-unsigned-char fft-unsigned-short fft-unsigned-int fft-unsigned-long-long
                                 fft-signed-int))
(try name function (list 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0))
(try name function (list 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1))
(try name function (list F F F F F F F F F F F F F F F F F F))
(try name function (list -1 -1 -1 -1 -1 -1 -1 -1 -1  -1 -1 -1 -1 +2 +2 +2 +2 -42))
(try name function (list +1 +2 +3 +4 +5 +6 +7 +8 +9
                         INT8_MAX INT16_MAX INT32_MAX INT64_MAX  UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX
                         42))
(try name function (list -1 -2 -3 -4 -5 -6 -7 -8 -9
                         INT8_MIN INT16_MIN INT32_MIN INT64_MIN  UINT8_MAX UINT16_MAX UINT32_MAX UINT64_MAX
                         -42))
