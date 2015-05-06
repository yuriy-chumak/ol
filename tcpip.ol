(import (owl render))

;(render obj '())

;                        (write-bytes stderr 
;                           (foldr render '(10) 
;                              (list "mandatory option not given: " (get rule 'long "(missing)"))))
;                        #false))



(define SOCK_STREAM 1)
(define SOCK_DGRAM 2)
(define SOCK_RAW 3)

(define (socket)      (sys-prim 41 #f #f #f))
(define (bind s port) (sys-prim 49 s port #f))
(define (listen sock) (sys-prim 50 sock #f #f))
(define (accept sock) (sys-prim 43 sock #f #f))

(define (shutdown sock) (sys-prim 48 sock #f #f))

(define (send sock . d) (sys-prim 44 sock (raw type-vector-raw (foldr render '() d)) #f))
(define (recv sock max) (cast (sys-prim 0 sock max #false) type-string))
;(define (sendnl sock . d) (sys-prim 44 sock (raw type-vector-raw (foldr render '(13 10) d)) #f))


(define s (socket))
(print (bind s 8822))
(print (listen s))

(define (loop)
   (define l (accept s))
   (print (recv l 4096))
   (send l "HTTP/1.x 200 OK\r\n\r\n")
   (let* ((ss ms (clock)))
      (send l "now: " ss "s " ms "ms"))
   (shutdown l)
   (loop))
(loop)
(shutdown s)

