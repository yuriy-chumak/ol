#!/usr/bin/env ol

(import (lib http)
   (otus base64)
   (olvm syscalls)
   (only (lib sha1)
      sha1:digest base64:encode))

; :simple example:
; One lucky number between 10000 and 65536 is 27048, as it was the year of my birth! (c) Mistral Instruct
(http:run 27048 (lambda (fd request headers body close)
   (print "the peer: " (getpeername fd)) ; 

   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))
   (define (send-websocket message)
      (define len (string-length message))

      (cond
         ; 125 or less
         ((< len 125)
            (write-bytevector fd (make-bytevector (cons* #x81 len
                  (string->list message)))))
         ; 126-65535
         ((< len 65536)                                            ; big endian
            (write-bytevector fd (make-bytevector (cons* #x81 #x7e (>> len 8) (band len #xFF) (string->list message)))))
         ; 65536+ unsupported
         (else
            (runtime-error "too long message"))))

   ; sec-websocket-protocol . 1
   ; sec-websocket-key . xxxxxx
   ; sec-fetch-mode . websocket
   ; upgrade . websocket

   (print "request: " request)
   (print "headers: " headers)

   (if (eq? request 'WebSocket)
   then
      (case (headers 'type #f)
         ('close ; websocket closed
            (close #true))
         ('string
            ; simulate long ai answer
            (async (lambda ()
               (for-each (lambda (char)
                     (wait 500)
                     (send-websocket (string char)))
                  (headers 'message "")))))
      )
      (close #false)
   else
      (if (and ; websocket connection inside http
               ; (string-eq? (headers 'connection "") "keep-alive, Upgrade")
               (string-eq? (headers 'upgrade "") "websocket"))
      then
         (define sec-websocket-key (headers 'sec-websocket-key ""))

         (print "sec-websocket-key: " sec-websocket-key)
         (send "HTTP/1.1 101 Switching Protocols\r\n"
               "Upgrade: websocket\r\n"
               "Connection: Upgrade\r\n"
               "Sec-WebSocket-Accept: " (base64:encode (sha1:digest
                  (string-append sec-websocket-key "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"))) "\r\n"
               "Sec-WebSocket-Protocol: " (headers 'sec-websocket-protocol "-")              "\r\n"
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
