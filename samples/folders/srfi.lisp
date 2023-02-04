#!/usr/bin/env ol

(import (srfi 170))

(define dir (open-directory "."))
(let loop ()
   (define filename (read-directory dir))
   (when filename
      (print "filename: " filename)
      (loop)))
(close-directory dir)
