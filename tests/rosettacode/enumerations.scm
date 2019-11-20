; https://www.rosettacode.org/wiki/Enumerations#Ol

(define *interactive* #true)
(define fruits '{
   apple  0
   banana 1
   cherry 2})
; or
(define fruits {
   'apple  0
   'banana 1
   'cherry 2})

; getting enumeration value:
(get fruits 'apple -1) ; ==> 0
; or simply
(fruits 'apple)        ; ==> 0
; or simply with default (for non existent enumeration key) value
(fruits 'carrot -1)    ; ==> -1

; simple function to create enumeration with autoassigning values
(define (make-enumeration . args)
   (fold (lambda (ff arg i)
            (put ff arg i))
      #empty
      args
      (iota (length args))))

(make-enumeration 'apple 'banana 'cherry)
