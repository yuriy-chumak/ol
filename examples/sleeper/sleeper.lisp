#!/usr/bin/env ol

(import (owl io))  ; reimport i/o lib to use a smart (wait-mail)

; a 1000 threads
; every thread waits for x seconds and prints a number
(for-each
   (lambda (x)
      (actor x (lambda ()
         (let loop ()
            (define envelope (wait-mail (* x 1000)))
            (unless envelope
               (for-each display (list "[" x "]"))
               (loop))) )))
   (iota 1000 1))

; echo tcp server, try it with "telnet 127.0.0.1 12321"
(import (olvm syscalls))

(define (on-accept fd)
   (lfor-each (lambda (char)
         (write-char char fd))
      (port->bytestream fd))
   (print "client disconnected"))

(define (echo:run port)
   (define ss (socket))
   (bind ss port)    (listen ss)
   (let loop ()
      (when (wait-read ss 30000) ; 30 sec
         (let ((fd (accept ss)))
            (print "\n# " (strftime "%c") ": new request from " (getpeername fd))
            (async (lambda () (on-accept fd)))))
      (loop)) )

(echo:run 12321)
