;; Start 100K threads and pass a message through them

(define n 100000)

; simply forwards a message to the next coroutine
(define (forwarder to)
	(lambda ()
		(let loop ()
			(mail to (ref (wait-mail) 2))
			(loop))))

(print "Starting " n " threads.")

;; 2-(n-1) just forward to the next
(let loop ((id (- n 1)) (next n))
	(if (> id 1)
		(begin
			(actor id (forwarder next))
			(loop (- id 1) id))))

; first one is a special,
;  wait for a message, send "pass this around" to the second coroutine
;  print a secnd message and stop execution
(actor 1 (lambda ()
   (define msg (ref (wait-mail) 2))
   (print "  actor 1 got '" msg "', let's go!")
   (mail 2 "pass this around")

   (let*((envelope (wait-mail))
         (sender msg envelope))
      (print "Thread 1: actor named '" sender "' sent me an '" msg "'"))))

;; last one sends to first
(print "Sending a first message.")
(actor n (forwarder 1))

(mail 1 'start)
