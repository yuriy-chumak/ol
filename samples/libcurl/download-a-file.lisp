#!/usr/bin/env ol

(import (lib curl))

(define file (open-output-file "quine.lisp"))
(unless file
   (print "can't create or open file")
   (halt 1))
(define mmap (lambda (ptr size)
   (syscall 9 ptr size)))

(import (otus ffi))
(define write (vm:pin (ilist
   (list fft-int fft-void* fft-int fft-int fft-void*)
   (lambda (ptr sz nmemb userdata)
      (write-bytevector (mmap ptr (* sz nmemb)) file)
      (* sz nmemb)))))

(define curl (make-curl))
(curl 'url "https://raw.githubusercontent.com/yuriy-chumak/ol/master/quine.lisp")
(curl 'writefunction (make-callback write))
(curl 'perform) ;; or just (curl)
(close-port file)