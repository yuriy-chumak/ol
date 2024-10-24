#!/usr/bin/env ol

; a 1000 threads
; every thread waits for x seconds and prints a number
(for-each
   (lambda (x)
      (async x (lambda ()
         (let loop ((old (time-ms)))
            (define envelope (check-mail))
            (unless envelope
               (define now (time-ms))
               (if (> (- now old) (* x 1000))
               then
                  (for-each display (list "[" x "]"))
                  (loop now)
               else
                  (sleep 99)
                  (loop old)))) )))
   (iota 1000 1))

; echo tcp server, try it with "telnet 127.0.0.1 32123"
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
      (if (select ss
            (if (null? (running-threads)) 3000000 1))
         (let ((fd (accept ss)))
            (print "\n# " (strftime "%c") ": new request from " (getpeername fd))
            (async (lambda () (on-accept fd)))))
      (sleep 0) ; we need it!
      (loop)) )

(echo:run 32123)
