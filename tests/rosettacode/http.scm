; http://www.rosettacode.org/wiki/HTTP#Ol

(import (lib curl))

(define curl (make-curl))
(curl 'url "http://rosettacode.org/")
(curl 'perform)
