(define (infinite? x) (or (equal? x +inf.0) (equal? x -inf.0)))

(print (infinite? +inf.0)) ; #true
(print (infinite? -inf.0)) ; #true
(print (infinite? +nan.0)) ; #false
(print (infinite? 123456)) ; #false
(print (infinite? 1/3456)) ; #false
(print (infinite? 17+28i)) ; #false
