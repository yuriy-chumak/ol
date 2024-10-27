#!/usr/bin/env ol

; socket syscalls
(import (olvm syscalls))

; date like Sun Oct 27 11:35:00 2024
(define (timestamp) (strftime "%c"))

; echo server
(define (on-accept fd)
   (lfor-each (lambda (char)
         (write-char char fd))
      (port->bytestream fd))
   (print "client disconnected"))

(define (echo:run port)
   (let ((conn (socket)))
      ; bind to port
      (unless (bind conn port)
         (runtime-error "Can't bind to port" port))
      ; listen
      (unless (listen conn)
         (runtime-error "Can't listen socket"))

      (print "listening 127.0.0.1:" port)
      (print "use 'telnet 127.0.0.1 " port "' to connect and test")
      ; accept
      (let loop ()
         (when (wait-read conn 30000) ; 30s
            (let ((fd (accept conn)))
               (print "\n# " (timestamp) ": new request from " (getpeername fd))
               (async (on-accept fd))))
         (loop))))

(echo:run 12321)
