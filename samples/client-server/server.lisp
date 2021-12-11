#!/usr/bin/env ol

; commands
(define MOVE 1001)
(define GET 1002)

(coroutine 'hero (lambda ()
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
(define (on-accept socket)
   (print "client " (syscall 51 socket) " connected.")

   ; greeting
   (write-bytestream (fasl-encode "I see you, please wait...") socket)
   
   (define command (fasl-decode (port->bytestream socket) #f))
   (print "  received a command " (if command command "invalid command"))
   
   (define answer (await (mail 'hero command)))
   (print "  hero answered " answer)
   
   (write-bytestream (fasl-encode (await (mail 'hero [GET]))) socket)

   (print "  current hero status is " (await (mail 'hero [GET]))))


; let's start tcp server
(define socket (syscall 41))
(define port 8888)

; bind
(unless (syscall 49 socket port)
   (print-to stderr "can't bind to a port " port)
   (exit 1))
(print-to stderr "server binded to " port)

; listen
(unless (syscall 50 socket)
   (print-to stderr "can't listen a socket")
   (exit 2))
(print-to stderr "server listening to 0.0.0.0:" port)

; accept loop
(let loop ()
   (if (syscall 23 socket
         (if (null? (running-threads)) 3000000 1)) ; wait a 3 seconds if no running threads detected
      (let ((fd (syscall 43 socket))) ; accept
         (async (on-accept fd)))
      (sleep 0)) ; else just switch context
   (loop)) )))


(close-port socket)

(print "done.")