#!/usr/bin/env ol

(import (owl parse))
(import (file gzip))

(define gzfilename (or
   (lref *command-line* 0)
   "input.tar.gz"))

(print "filename: " gzfilename)
(define file (try-parse gzip-parser (file->bytestream gzfilename) #false))
; (car file): decoded GZIP, https://datatracker.ietf.org/doc/html/rfc1952
;  'FLG:      internal flags
;  'OS:       (Operating System)
;  'FNAME:    file name
;  'stream:   ungzipped stream
; (cdr file): gzipped stream

(if file
   (print (bytes->string ((car file) 'stream)))
else
   (print "stream is not gzipped"))
(print)