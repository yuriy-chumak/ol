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

(define (connect sock host port) (sys-prim 42 sock host port))
(define (shutdown sock) (sys-prim 48 sock #f #f))

(define (send sock . d) (sys-prim 1 sock (raw type-string (foldr render '() d)) -1))
(define (recv sock max) (sys-prim 0 sock max #false))
;(define (sendnl sock . d) (sys-prim 44 sock (raw type-vector-raw (foldr render '(13 10) d)) #f))



(define s (socket))
(print (connect s "google.com" 80))
(print (send s "GET / HTTP/1.1\r\n\r\n"))
(print (cast (recv s 2048) type-string))

(define rcode (s/







(shutdown s)
