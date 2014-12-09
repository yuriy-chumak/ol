;;;
;;; Simple direct blocking IO (replaces the old thread-based one)
;;;

(print "Start:")

; http://www.linuxhowtos.org/C_C++/socket.htm

(define (fail message) (error message #f))

(define (write s d l) (sys-prim 1 s d l))
(define (close handle) (sys-prim 3 handle #f #f))

;;;; sockets
(define SOCK_STREAM 1)
(define SOCK_DGRAM 2)


(define (socket type) (sys-prim 41 type #false #false))
(define (bind s port) (sys-prim 49 s    port   #false))
(define (listen sock) (sys-prim 50 sock #false #false))
(define (accept sock) (sys-prim 43 sock #false #false))
(define (send sd d l) (sys-prim 44 sd   d      l))
(define (shutdown sd) (sys-prim 48 sd   #false #false))


;(define (new-tcp-socket)
;   (let ((sock (socket SOCK_STREAM)))
;      (if (sock)
;          (cons 'socket sock))))
;;          #f)))




(define server (socket SOCK_STREAM))
(if (not server)
   (fail "Can't create server socket"))

(if (not (bind server 80))
   (fail "Can't bind server socket"))
    
(if (not (listen server))
   (fail "Can't listen server socket"))

(define message "HTTP/1.1 200 OK\nContent-Type: text/html; charset=utf-8\n\n")
(define message-length (string-length message))

; server example
(define (loop)
   (print "Wait for client...")
   (let ((s (accept server)))
      (if (not s)
         (fail "Can't accept server socket"))
         
      (print "Client got. Let's send some msg")
      (if (not (send s message message-length))
         (fail "Can't send data to client"))
         
      (send s "hallo :)" 8)
      (close s)
      (loop)))
(loop)
   
   

(print message)

(print "next accept")
(define s (accept server))
(if (not s)
   (fail "Can't accept server socket"))
