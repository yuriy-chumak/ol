; http://rosettacode.org/wiki/Interactive_programming#Ol

(define *interactive* #t)
(define (f head tail mid)
     (string-append head mid mid tail))
(f "Rosetta" "Code" ":")
,quit
