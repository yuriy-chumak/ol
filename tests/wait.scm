#!/usr/bin/env ol

(actor 'timer (lambda ()
   (let loop ((n 0))
      (unless (check-mail)
         (print n)
         (wait 100) ; 1 second wait
         (loop (+ n 1))))))

(wait 520)
(mail 'timer #n) ; stop timer and autoexit program
