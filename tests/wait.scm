#!/usr/bin/env ol

(actor 'timer (lambda ()
   (let loop ((n 0))
      (unless (check-mail)
         (print n)
         (wait 100) ; 100 ms wait
         (loop (+ n 1))))))

(wait 540)
(mail 'timer #n) ; stop timer and autoexit program
