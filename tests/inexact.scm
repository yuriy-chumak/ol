(define x (inexact 4))
(for-each print (list
   (eq? x 4)                ; #false
   (= x 4)                  ; #true
   (= 4 x)                  ; #true
   (eq? x (inexact 4))      ; #false
   (eq? (inexact 4) x)      ; #false
   (equal? x 4)             ; #false
   (equal? 4 x)             ; #false
   (equal? x (inexact 4))   ; #true
   (equal? (inexact 4) x)   ; #true
))