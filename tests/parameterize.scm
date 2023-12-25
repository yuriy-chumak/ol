#!/usr/bin/env ol

(define buffer (open-output-string))
(parameterize ((current-output-port buffer))
   (display "hello"))
(print "the output string is '" (get-output-string buffer) "'")
