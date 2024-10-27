#!/usr/bin/env ol

(import (otus random!))
(import (olvm syscalls))

; commands
(define MOVE 1001)

; --------------------------
; let's start tcp server
(define conn (socket))

(define address "127.0.0.1")
(define port 8888)

; connect
(unless (connect conn address port)
   (print-to stderr "can't connect to a " address ":" port)
   (exit 1))
(print-to stderr "server connected to a " address ":" port)

; wait for greeting:
(print "greeting: " (fasl-decode (port->bytestream conn) #f))

; write a sample command (move hero to point 3 5)
(define command (fasl-encode [MOVE (- (rand! 5) 2) (rand! 7)]))
(print "  sending command " command)
(write-bytestream command conn)

(define answer (fasl-decode (port->bytestream conn) #f))
(print "  hero position is: " answer)

(close-port conn)
(print "done.")