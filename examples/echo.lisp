#!/usr/bin/env ol

; echo server

(define (create-socket) (syscall 41))
(define (bind socket port) (syscall 49 socket port))
(define (listen socket) (syscall 50 socket))
(define (select socket) (syscall 23 socket))
(define (accept socket) (syscall 43 socket))
(define (timestamp) (syscall 201 "%c"))
(define (sendfile in out size) (syscall 40 in out 0 size))


(define (on-accept fd)
(lambda ()
   (let loop ((
   (sendfile stdin stdout 999)))

(define (echo:run port)
(let ((socket (create-socket)))
   ; bind
   (if (not (bind socket port)) ; bind
      (runtime-error "Can't bind to port" port))
   ; listen
   (if (not (listen socket)) ; listen
      (runtime-error "Can't listen socket"))

   ; accept
   (let loop ()
      (if (select socket) ; select
         (let ((fd (accept socket))) ; accept
            (print "\n# " (timestamp) ": new request from " (syscall 51 fd))
            (async (on-accept fd))))
      (sleep 0)
      (loop))))

(echo:run 12321)
