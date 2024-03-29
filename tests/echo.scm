
;;; a simple mail test: send 100 messages and have them echoed back from another thread

(define echo "echo server")

;; function to be forked to run as the echo thread
(define (echoer)
   (let*((envelope (wait-mail))
         (from msg envelope))
      (mail from msg)
      (echoer)))

(actor echo echoer)

(let loop ((n 1))
   (cond
      ((not (eq? n (await (mail echo n)))) (print "error"))
      ((= n 100) (print n))
      (else (loop (+ n 1)))))
