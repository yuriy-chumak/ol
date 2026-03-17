#!/usr/bin/env -S ../../ffi ../../repl
(import (otus ffi))

(when (or
         (and (eq? fft-long fft-int)
            (eq? fft-unsigned-long fft-unsigned-int)
            (eq? fft-signed-long fft-signed-int))
         (and (eq? fft-long fft-long-long)
            (eq? fft-unsigned-long fft-unsigned-long-long)
            (eq? fft-signed-long fft-signed-long-long)) )
   (print "all longs are either ints or longlongs. i've verified. right now!"))
