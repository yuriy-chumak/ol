; http://rosettacode.org/wiki/Read_entire_file#Ol

; scheme way
(import (scheme file))
(define content
   (call-with-input-file "LICENSE"
      (lambda (port)
         (read-string #f port))))

(display content)

; simpler way
(define content (file->string "LICENSE"))
(display content)
