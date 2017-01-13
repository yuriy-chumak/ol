#!/usr/bin/ol

; http://serverfault.com/questions/112795/how-can-i-run-a-server-on-linux-on-port-80-as-a-normal-user
; # iptables -t nat -A PREROUTING -p tcp --dport 80 -j REDIRECT --to-port 8080

(import (lib http))

; syscalls
(define (yield) (syscall 1022 0 #false #false))
(define (mem-stats) (syscall 1117 #f #f #f))
(define (time format seconds) (syscall 201 format seconds #f))
(define uname (syscall 63 0 0 0))

(define (concat . args) (foldr str-app "" args))

; some constants for uptime
(define (div a b) (floor (/ a b)))
(define M 60)
(define H (* M 60))
(define D (* H 24))

; send file to output stream
(define (sendfile fd content-type filename)
   (print "Sending as '" content-type "' " filename)
(let*((path (if (string? filename) (str-app "." (c-string filename)) "?"))
      (send (lambda args
         (for-each (lambda (arg)
            (display-to fd arg)) args)))
      (stat (syscall 4 path #f #f)))
   (if stat (begin
      (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)

      (send "HTTP/1.0 200 OK\r\n"
            "Connection: close\r\n"
            "Content-Type: " content-type "\r\n"
            "Content-Length: " (ref stat 8) "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n\r\n")
      (let ((file (syscall 2 path 0 #f))
            (size (ref stat 8)))
         (syscall 40 fd file size)
         (syscall 3 file #f #f))
      (print "File sent."))
   ;else
   (begin
      (cond
         ((eq? filename 404)
            (print "Sending 404 Not Found")
            (send "HTTP/1.0 404 Not Found\r\n"))
         ((eq? filename 405)
            (print "Sending 405 Method Not Allowed")
            (send "HTTP/1.0 405 Method Not Allowed\r\n"))
         (else
            (print "Sending 404 Not Found")
            (send "HTTP/1.0 404 Not Found\r\n")))
      (send "Connection: close\r\n"
            "Content-Type: text/html\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n\r\n")
      (send "<HTML><BODY>"
            "<h1>404 Not Found OK</h1>"
            "<h4>url: " filename "</h4>")))))


(define has-two-dots? (string->regex "m/\\.\\./"))
(define (starts-with string sub)
   (if (> (string-length sub) (string-length string))
      #false
      (string-eq? (substring string 0 (string-length sub)) sub)))

(define (str-find str char)
   (let loop ((string (str-iter str)) (n 0))
      (if (null? string)
         -1
         (if (char=? (car string) char)
            n
            (loop (force (cdr string)) (+ n 1))))))

(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
   (print "Headers: " headers)

   (let ((peer (syscall 51 fd #f #f)))
   (cond
      ((and (eq? (size request) 3)
            (string-eq? (ref request 1) "GET"))
         (let ((url (ref request 2)))
            (cond
               ;basic sanity check:
               ((has-two-dots? url)
                  (sendfile fd "text/html" 404))

               ;about
               ((or
                  (string-eq? url "/")
                  (starts-with url "/index.htm"))
                     (sendfile fd "text/html" "/index.html"))
               ((starts-with url "/olvm.js")
                  (sendfile fd "text/javascript" "/olvm.js"))
               ((starts-with url "/olvm.html")
                  (sendfile fd "text/html" "/olvm.html"))
               ((starts-with url "/olvm.html.mem")
                  (sendfile fd "text/javascript" "/olvm.html.mem"))

               ;else
               (else
                  (sendfile fd "text/html" 404)))))
      (else
          (sendfile fd "text/html" 404)))
   (close #t))))
