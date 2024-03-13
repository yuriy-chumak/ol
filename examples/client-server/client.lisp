#!/usr/bin/env ol

(import (otus random!))

; commands
(define MOVE 1001)

; --------------------------
; let's start tcp server
(define socket (syscall 41))

(define address "127.0.0.1")
(define port 8888)

; connect
(unless (syscall 42 socket address port)
   (print-to stderr "can't connect to a " address ":" port)
   (exit 1))
(print-to stderr "server connected to a " address ":" port)

; wait for greeting:
(print "greeting: " (fasl-decode (port->bytestream socket) #f))

; write a sample command (move hero to point 3 5)
(define command (fasl-encode [MOVE (- (rand! 5) 2) (rand! 7)]))
(print "  sending command " command)
(write-bytestream command socket)

(define answer (fasl-decode (port->bytestream socket) #f))
(print "  hero position is: " answer)

(close-port socket)
(print "done.")