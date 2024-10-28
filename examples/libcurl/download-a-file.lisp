#!/usr/bin/env ol

(import (lib curl))

(define mmap (lambda (ptr size)
   (syscall 9 ptr size)))

(import (otus ffi))
(define write (vm:pin (cons
   (cons fft-int (list fft-void* fft-int fft-int fft-void*))
   (lambda (ptr sz nmemb userdata)
      (bytevector->file (mmap ptr (* sz nmemb)) "quine.lisp")
      (* sz nmemb)))))

(define curl (make-curl))
(curl 'url "https://raw.githubusercontent.com/yuriy-chumak/ol/master/quine.lisp")
(curl 'writefunction (make-callback write))
(curl 'perform) ;; or just (curl)
