; http://rosettacode.org/wiki/Interactive_programming#Ol

(define *interactive* #t)
(define (f head tail mid)
     (fold string-append "" (list head mid mid tail)))
(f "Rosetta" "Code" ":")
,quit
