#!/bin/ol

(import (lib http)
        (lib sha1))
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
(define (yield) (sys-prim 1022 0 #false #false))
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
;(define (sublist l n) 
;   (if (or (null? l) (= n 0)) null (cons (car l) (sublist (cdr l) (- n 1)))))
(fork-server 'memory-stats-collector (lambda ()
(let loop ((stats '()) (seconds 0))
   (let ((envelope (check-mail)))
      (if envelope
         (let* ((sender send envelope)) ; todo: send here (send) lambda and do not move over bus all "stats" table
            (let do ((stat stats))
               (if (not (null? stat))
                  (begin
                     (send (caar stat))
                     (for-each (lambda (x) (send "\t" x)) (cdr (car stat)))
                     (send "\n")
                     (do (cdr stat)))))
            (mail sender stats))))
   (yield)
   (let ((ss (time #false #f)))
      (if (> (- ss seconds) 2)
         (let ((mem (mem-stats)))
            (loop (cons (cons (time "%d%H%M%S\t" ss) mem) stats) ss))
         (loop stats seconds))))))


(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
   (print "Headers: " headers)

   (let* ((url (ref request 2))
          (url (if (string-eq? url "/") "/index.html" url)))
      (cond
         ((string-eq? url "/ws")
            (let ((Upgrade (getf headers 'Upgrade)))
               (if Upgrade (begin
                  (print "id: " (string-append (getf headers 'Sec-WebSocket-Key) "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))
                  (print "answer: " (base64:encode (sha1:digest (string-append (getf headers 'Sec-WebSocket-Key) "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))))
                  (send "HTTP/1.1 101 Switching Protocols\n")
                  (send "Connection: Upgrade\n")
                  (send "Origin: null\n")
                  (send "Sec-WebSocket-Protocol: echo-protocol\n")
                  (send "Sec-WebSocket-Accept: " (base64:encode (sha1:digest (string-append (getf headers 'Sec-WebSocket-Key) "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))) "\n")
                  (send "Upgrade: websocket\n")
                  (send "Server: OL/1.0\n")
                  (send "\n")
                  ;else
                  (send "HTTP/1.1 200 OK\nServer: OL/1.0\n\n-"))))

         ((string-eq? url "/index.html")
            (send "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n")
            (send "Server: OL/1.0\n")
            (send "\n")

            (send (runes->string (file->list (str-app "./tutorial/2. Web Server" url))))
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
                        (ref (*version*) 1) "/" (ref (*version*) 2)
                        ", "
                        (ref uname 1) " " (ref uname 5)
                     "</div>")
               (send "<div style='clear: both'></div></small></pre>"))
            (close #t))
         ((string-eq? url "/data.tsv")
            (send "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n")
            (send "Server: OL/1.0\n")
            (send "\n")
            
            (send "date\tGeneration\tAllocated\tTotal Size\t\n")
            (interact 'memory-stats-collector send)
            (close #t))
         (else
            (send "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n")
            (send "Server: OL/1.0\n")
            (send "\n")
            
            (send "<HTML><BODY>")
            (send "<h1>url:" url "</h1>")
            (send "<hr>")
            ; uptime:
            (let* ((sysinfo (syscall 99 #f #f #f))
                   (uptime (ref sysinfo 1)))
               (send "<small>"
                     "uptime: "
                              (div      uptime D   ) " days, "
                              (div (rem uptime D) H) " hr, "
                              (div (rem uptime H) M) " min, "
                              (     rem uptime M   ) " sec."
                     "</small>"))
            (send "</BODY></HTML>")
            (close #t))))))
))
