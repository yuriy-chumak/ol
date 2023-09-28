#!/usr/bin/env ol

(actor 'main (lambda ()
   (define detouched1 (async-linked
      (lambda ()
         (* 1 2 3 4 5 6))))
   (print detouched1)
   (define detouched2 (async-linked
      (lambda ()
         (* 1 2 3 4 5 6 7 8 9))))
   (print detouched2)

   (print 1)
   (sleep 100)
   (print 2)
   (sleep 100)
   (print 3)

   (define result (await detouched1))
   (print "result: " result)
   (define result (await detouched2))
   (print "result: " result)
))
