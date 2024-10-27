#!/usr/bin/env ol

; http://serverfault.com/questions/112795/how-can-i-run-a-server-on-linux-on-port-80-as-a-normal-user
; # iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080

(import
   (lib http)
   (file json)
   (owl parse)
   (only (file parser) times)
   (only (olvm syscalls) strftime))

(http:run 8080 (lambda (fd request headers body close)
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))

   (print "fd: " fd ", request: " request)
   (print "User-Agent: \e[0;36m" (get headers 'user-agent #false) "\e[0;0m")

   (define content-length (headers 'content-length "0"))
   (define content-length-num (string->number content-length))
   (print "content-length-num: " content-length-num)
   (if content-length
   then
      (define data (try-parse
            (let-parse*(
                  (bytes (times content-length-num byte)))
               bytes)
         body #false))
      (if data
      then
         (send "HTTP/1.0 200 OK\n"
               "Connection: close\n"
               "Content-Type: " "text/html" "\n"
               "Connection: close" "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\n\n"
         ; just add field 'processed to the json
         data)
      else
         (send "HTTP/1.0 200 OK\n"
               "Connection: close\n"
               "Content-Type: " "text/html" "\n"
               "Connection: close" "\r\n"
               "Server: " (car *version*) "/" (cdr *version*) "\n\n"
               "Invalid data body"))
   else
      (send "HTTP/1.0 200 OK\n"
            "Connection: close\n"
            "Content-Type: " "text/html" "\n"
            "Connection: close" "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n"
            "No data in the input stream."))
   (close #t)))
