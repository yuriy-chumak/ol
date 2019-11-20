; https://rosettacode.org/wiki/Object_serialization#Ol
(define Object [
   '(1 2 3 4)  ; list
   #(4 3 2 1)  ; vector
   "hello"     ; ansi string
   "こんにちは"; unicode string
   (pairs->ff '(; associative array
      (1 . 123456)
      (2 . second)
      (3 . "-th-")))
   {(4 . 'sym) ; alternatively declared..
    (5 . +)}   ; ..associative array
   #false      ; value
   -123        ; short number
   123456789012345678901234567890123456789  ; long number
])

(print Object)
(fasl-save Object "/tmp/object.bin")
(define New (fasl-load "/tmp/object.bin" #false))
(print New)
(print (equal? Object New))
