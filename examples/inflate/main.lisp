#!/usr/bin/env ol

(import (data parse))
(import (file gzip))

(define gzfilename (car *command-line*))

(print "filename: " gzfilename)
(define file (parse gzip-parser (file->bytestream gzfilename)))
; (car file): decoded GZIP, https://datatracker.ietf.org/doc/html/rfc1952
;  'FLG:      internal flags
;  'OS:       (Operating System)
;  'FNAME:    file name
;  'stream:   ungzipped stream
; (cdr file): gzipped stream

(if file
   (print (bytes->string (file 'stream)))
   (print "stream is not gzipped"))
(print)