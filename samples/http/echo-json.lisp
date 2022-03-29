#!/usr/bin/env ol

; http://serverfault.com/questions/112795/how-can-i-run-a-server-on-linux-on-port-80-as-a-normal-user
; # iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080

(import
   (lib http)
   (file json)
   (owl parse)
   (only (otus syscall) strftime))

; syscalls
(define (yield) (syscall 1022 0))

(http:run 8080 (lambda (fd request headers body close)
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))

   (print "fd: " fd ", request: " request)
   (print "User-Agent: \e[0;36m" (get headers 'user-agent #false) "\e[0;0m")

   (if (string-eq? (headers 'content-type "") "application/json")
   then
      (define json (try-parse json-parser body #false))
      (if json
      then
         (send "HTTP/1.0 200 OK\n"
               "Connection: close\n"
               "Content-Type: " "application/json" "\n"
               "Connection: close" "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\n\n")
         ; just add field 'processed to the json
         (define new-json (put (car json) 'processed (strftime "%c")))
         (print-json-with send new-json)
      else
         (send "HTTP/1.0 200 OK\n"
               "Connection: close\n"
               "Content-Type: " "text/html" "\n"
               "Connection: close" "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\n\n")
         (send "Invalid json body"))
   else
      (send "HTTP/1.0 200 OK\n"
            "Connection: close\n"
            "Content-Type: " "text/html" "\n"
            "Connection: close" "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
      (send "No json in the input stream."))
   (close #t)))
