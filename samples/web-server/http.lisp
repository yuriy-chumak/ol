#!/usr/bin/env ol

(import (lib http))
; simplest example:
(http:run 8080 (lambda (fd request headers body close)
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))
   (print ":: " (syscall 51 fd))
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*)
         "\n\n"

         "<h1>200: OK</h1>"
         (ref request 1) ": " (ref request 2) " &gt; " (http:parse-url (ref request 2))
         "<hr><small>" headers
         "</small>")
   (close #t)
))
