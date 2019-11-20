; http://rosettacode.org/wiki/JSON#Ol

(import (lib json))
(import (owl parse))

(define o (parse json-parser (str-iter "{'something':[12,23,34],'new':true}") #f #f #f))
(print o)

(print-json-with display o)
(print-json-with display {
   'a  [1 2 3]
   'b  {1 2  3 4}
   'c  #true})
