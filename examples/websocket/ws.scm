#!/usr/bin/env ol

; websocket library
;; webSocket = new WebSocket(url, protocols);

(import (lib http)
   (otus base64)
   (lib sha1)) ; sha1:digest and base64:encode

; simplest example:
; One lucky number between 10000 and 65536 is 27048, as it was the year of my birth! (c) Mistral Instruct
(http:run 27048 (lambda (fd request headers body close)
   (print ":: " (syscall 51 fd))

   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))
   (define (send-websocket message)
      (define bytes (string->list message))
      (print "sending message " message)
      (write-bytes fd (cons* #x81 (length bytes) bytes)))
      ;(write-bytes fd (cons* #x81 0)))
      ;)

   ; sec-websocket-protocol . 1
   ; sec-websocket-key . xxxxxx
   ; sec-fetch-mode . websocket
   ; upgrade . websocket

   (print "request: " request)
   (print "headers: " headers)
   (print "body: " body)

   (if (eq? request 'WebSocket)
   then
      ;(print "> " (list->string headers))
      (case (headers 'type #f)
         ('close ; websocket closed
            (close #true))
         ('string
            ; simulate ai network answer
            (async (lambda ()
               (let loop ((n 1))
                  (send-websocket (string-append (number->string n) " "))
                  (sleep 9999)
                  (if (< n 10)
                     (loop (+ n 1))))
               (send-websocket "END."))))
      )
      (close #false)
   else
      (if (and ; websocket connection inside http
               ; (string-eq? (headers 'connection "") "keep-alive, Upgrade"), todo: find "upgrade" in 'connection header
               (string-eq? (headers 'upgrade "") "websocket"))
      then
         (define sec-websocket-key (headers 'sec-websocket-key ""))

         (print "sec-websocket-key: " sec-websocket-key)
         (send "HTTP/1.1 101 Switching Protocols\r\n"
               "Upgrade: websocket\r\n"
               "Connection: Upgrade\r\n"
               "Sec-WebSocket-Accept: " (base64:encode (sha1:digest
                  (string-append sec-websocket-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))) "\r\n"
               "\r\n")
         (close #false)
      else
         (send "HTTP/1.0 400 Bad Request\r\n"
               "Connection: close\r\n"
               "Content-Type: text/html; charset=UTF-8\r\n"
               "Server: " (car *version*) "/" (cdr *version*)
               "\r\n\r\n"

               "<h1>200: OK</h1>"
               (ref request 1) ": " (ref request 2) " &gt; " (http:parse-url (ref request 2))
               "<hr><small>" headers
               "</small>")
         (close #true)))))

   ;; (close #t)
   ;; ))
