(define Object (tuple
   '(1 2 3 4)  ; list
   #(4 3 2 1)  ; bytevector
   "hello"     ; ansi string
   "こんにちは"; unicode string
   (list->ff '(; hash table
      (1 . 123456)
      (2 . second)
      (3 . "-th-")))
   #false      ; value
   -123        ; short number
   123456789012345678901234567890123456789  ; long number
))

(print Object)
(fasl-save Object "/tmp/object.bin")
(define New (fasl-load "/tmp/object.bin" #false))
(print New)
(print (equal? Object New))
