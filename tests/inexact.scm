(define x #i4)
(for-each print (list
   (eq? x 4)        ; #false
   (= x 4)          ; #true
   (= 4 x)          ; #true
   (eq? x #i4)      ; #false
   (eq? #i4 x)      ; #false
   (equal? x 4)     ; #false
   (equal? 4 x)     ; #false
   (equal? x #i4)   ; #true
   (equal? #i4 x)   ; #true
))
