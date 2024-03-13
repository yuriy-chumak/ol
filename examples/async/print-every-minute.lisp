#!/usr/bin/env ol

(async (lambda ()
   (let*((ss ms (clock)))
      (print "async started at " ss " epoch seconds")
      (let loop ((ss ss) (ms ms))
         (let*((ss2 ms2 (clock)))
            (if (>= (- ss2 ss) 60) ; 1 minute
            then
               (print "tick! " ss2 " epoch seconds")
               (loop ss2 ms2)
            else
               (sleep 10)
               (loop ss ms)))))))

