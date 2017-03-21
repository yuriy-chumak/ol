#!/usr/bin/ol
; todo: write post like http://jakeyesbeck.com/2015/10/11/building-a-simple-web-server-with-ruby-2/
; todo: check web sockets: https://github.com/meefik/websocket.sh
;       https://github.com/sourcelair/xterm.js

(import (lib http))
; simplest example:
;(http:run 8080 (lambda (request headers send)
;   (send "HTTP/1.1 200 OK\n")
;   (send "Content-Type: text/html; charset=UTF-8\n")
;   (send "Server: OL/1.0\n")
;   (send "\n")
;   (send "<h1>200: OK</h1>")
;   (send (car request) "<br>" (cadr request))
;   (send "<hr><small>" headers)
;))

; real world example:
; этот сервер будет делать вполне простую работу - показывать статистику использования памяти
; http://habrahabr.ru/company/yandex/blog/265569/

; syscalls
(define (mem-stats) (syscall 1117 #f #f #f))
(define (time format seconds) (syscall 201 format seconds #f))
(define uname (syscall 63 0 0 0))

; some constants for uptime
(define (div a b) (floor (/ a b)))
(define M 60)
(define H (* M 60))
(define D (* H 24))


; 1 раз в 30 секунд соберем статистику по использованию памяти
; todo: ограничить список N элементами
(define (sublist l n)
   (if (or (null? l) (= n 0)) null (cons (car l) (sublist (cdr l) (- n 1)))))
(fork-server 'memory-stats-collector (lambda ()
(let loop ((stats '()) (seconds 0))
   (let ((envelope (check-mail)))
      (if envelope
         (let* ((sender send  envelope))
            (print "sending stats data")
            (send "date\tGeneration\tAllocated\tTotal Size\t\n")
            (let do ((stat stats))
               (if (not (null? stat))
                  (begin
                     (send (caar stat))
                     (for-each (lambda (x) (send "\t" x)) (cdr (car stat)))
                     (send "\n")
                     (do (cdr stat)))))
            (print "data sent")
            (mail sender #t))))
   (yield)
   (let ((ss (time #false #f)))
      (if (> (- ss seconds) 2)
         (let ((mem (mem-stats)))
            (loop (cons (cons (time "%d%H%M%S\t" ss) mem) stats) ss))
         (loop stats seconds))))))

; send file to output stream
(define (sendfile fd content-type filename)
   (print "Sending as '" content-type "' " filename)
(let*((path (c-string (str-app "./static" filename)))
      (send (lambda args
         (for-each (lambda (arg)
            (display-to fd arg)) args)))
      (stat (syscall 4 path #f #f)))
   (if stat (let ((file (fopen path 0)))
               (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)
               (send "HTTP/1.0 200 OK\n"
                     "Connection: close\n"
                     "Content-Type: " content-type "\n"
                     "Content-Length: " (ref stat 8) "\n"
                     "Server: " (car *version*) "/" (cdr *version*) "\n\n")
               (syscall 40 fd file (ref stat 8))
               (fclose file)
               (print "File sent."))
   ;else
   (begin
      (print "Sending 404 Not Found")
      (send "HTTP/1.0 404 Not Found\n"
            "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")))))

(define (send-uptime send)
   (send "<hr>")
   (send "<pre><small>")
   ; uptime:
   (let* ((sysinfo (syscall 99 #f #f #f))
          (rusage  (syscall 98 #f #f #f))
          (utime (ref rusage 1))
          (stime (ref rusage 2))
          (uptime (ref sysinfo 1)))
      (send
            "<div style='float:left'>"
            "uptime: "
                     (div      uptime D   ) " days, "
                     (div (rem uptime D) H) " hr, "
                     (div (rem uptime H) M) " min, "
                     (     rem uptime M   ) " sec."
            "<br>"
            "rusage: "
                     (div      (car utime) D   ) " days, "
                     (div (rem (car utime) D) H) " hr, "
                     (div (rem (car utime) H) M) " min, "
                     (     rem (car utime) M   ) " sec. "
                  " / "
                     (div      (car stime) D   ) " days, "
                     (div (rem (car stime) D) H) " hr, "
                     (div (rem (car stime) H) M) " min, "
                     (     rem (car stime) M   ) " sec."
            "</div>")
      (send "<div style='float:right'>"
            (time "%c" #false) ", Web Server: "
               (car *version*) "/" (cdr *version*)
               ", "
               (ref uname 1) " " (ref uname 5)
            "</div>")
      (send "<div style='clear: both'></div></small></pre>")))



(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
   (print "Headers: " headers)

   (cond
      ((string-eq? (ref request 1) "<policy-file-request/>")
         (print "*\n*\n*\n REQUESTED policy!!!")
         (write-vector (file->vector "./static/flashpolicy.xml") fd)
         (close #t))
      ((string-eq? (ref request 1) "GET")
         (let* ((url (ref request 2))
                (url (if (string-eq? url "/") "/index.html" url)))
            (cond
               ((string-eq? url "/ol")
                  (print "!!! ol")
                  (syscall 59 (c-string "/usr/bin/ol") (list (c-string "#") (c-string "-") (c-string "--seccomp")) (list fd fd fd))
                  (close #t))

               ((or
               (string-eq? url "/index.html")
               (string-eq? url "/meminfo.html"))
                  (sendfile fd "text/html" url)
                  (send-uptime send)
                  (close #t))

               ((or
               (string-eq? url "/d3.min.js")
               (string-eq? url "/jquery-1.11.3.min.js")
               (string-eq? url "/jquery.terminal-0.8.8.min.js")
               (string-eq? url "/jquery-tools-flashembed.js")
               (string-eq? url "/jsocket-1.0.0.min.js"))
                  (sendfile fd "application/javascript" url)
                  (close #t))

               ((string-eq? url "/jquery.terminal.css")
                  (sendfile fd "text/css" url) ; charset=UTF-8
                  (close #t))

               ((string-eq? url "/TCP.swf")
                  (sendfile fd "application/x-shockwave-flash" url)
                  (close #t))


               ((string-eq? url "/meminfo.tsv")
                  (print "Sending as 'text/plain' " url)
                  (send "HTTP/1.0 200 OK\n"
                        "Connection: close\n"
                        "Content-Type: text/plain\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n\n")
                  (interact 'memory-stats-collector send)
                  (close #t))

               (else
                  (print "Sending 404 Not Found")
                  (send "HTTP/1.0 404 Not Found\n"
                        "Connection: close\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n\n")

                  (send "<HTML><BODY>"
                        "<h1>404 Not Found OK</h1>"
                        "<h4>url: " url "</h4>")
                  (send-uptime send)
                  (close #t)))))))
))
