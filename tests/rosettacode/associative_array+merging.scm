; http://www.rosettacode.org/wiki/Associative_array/Merging#Ol

(define a1 {
   'name    "Rocket Skates"
   'price   12.75
   'color   "yellow"
})

(define a2 {
   'price   15.25
   'color   "red"
   'year    1974 
})

(print "a1: " a1)
(print "a2: " a2)

(define (collide a b) b) ; will use new key value
(print "merged a1 a2: " (ff-union a1 a2 collide))