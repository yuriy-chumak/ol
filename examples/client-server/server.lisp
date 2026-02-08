#!/usr/bin/env ol

(import (olvm syscalls))

; commands
(define MOVE 1001)
(define GET 1002)

(actor 'hero (lambda ()
(let loop ((this {
                  'x 10
                  'y 7
                 }))
(let*((envelope (wait-mail))
      (sender msg envelope))
   (case msg
      ([MOVE x y]
         (mail sender "moved")
         (loop (ff-fold (lambda (ff key value)
                           (put ff key value))
                  this
                  { 'x (+ (this 'x) x)
                    'y (+ (this 'y) y) })))
      ([GET]
         (mail sender {
            'x (this 'x #f)
            'y (this 'y #f)
         })
         (loop this))
      (else
         (print-to stderr "Unknown command " msg)
         (mail sender #false)
         (loop this)))))))

; clients handler
(define (on-accept conn)
   (print "client " (getpeername conn) " connected.")

   ; greeting
   (write-bytestream (fasl-encode "I see you, please wait...") conn)
   
   (define command (fasl-decode (port->bytestream conn) #f))
   (print "  received a command " (if command command "invalid command"))
   
   (define answer (await (mail 'hero command)))
   (print "  hero answered " answer)
   
   (write-bytestream (fasl-encode (await (mail 'hero [GET]))) conn)

   (print "  current hero status is " (await (mail 'hero [GET]))))


; let's start tcp server
(define conn (socket))
(define port 8888)

; bind
(unless (bind conn port)
   (print-to stderr "can't bind to a port " port)
   (exit 1))
(print-to stderr "server binded to " port)

; listen
(unless (listen conn)
   (print-to stderr "can't listen a conn")
   (exit 2))
(print-to stderr "server listening to 0.0.0.0:" port)

; accept loop
(let loop ()
   (when (wait-read conn 30000)
      (let ((fd (accept conn))) ; client trying to connect, accept connection
         (when fd
            (async (delay (on-accept fd)))))
      (sleep 0)) ; else just switch context
   (loop))


(close-port conn)

(print "done.")
