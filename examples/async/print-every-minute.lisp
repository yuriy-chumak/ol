#!/usr/bin/env ol

(import (only (olvm syscalls) strftime))

(async (lambda ()
   (print "async started at " (strftime "%c"))
   (let loop ()
      (wait 60000)
      (print "tick! " (strftime "%c"))
      (loop))))
