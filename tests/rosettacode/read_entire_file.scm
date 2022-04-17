; http://rosettacode.org/wiki/Read_entire_file#Ol

(define content (bytes->string
   (file->bytestream "LICENSE")))
(print content)
