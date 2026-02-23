; http server skeleton
; HTTP/1.0 - https://tools.ietf.org/html/rfc1945
(define-library (lib http2)
  (export
    http:run)
    ;http:parse-url)

  (import (scheme base) (srfi 1)
      (file http)
      (otus case-apply)
      (otus async)
      (scheme exceptions)

      (owl math) (owl list) (owl io) (owl string) (owl ff) (owl list-extra)
      (data s-exp))

(begin
   ; unique session id:
   (setq coname '[http-session-ids])

   (actor coname (lambda ()
   (let this ((id 1))
   (let*((envelope (wait-mail))
         (sender msg envelope))
      (mail sender id)
      (this (+ id 1))))))
   (define (generate-unique-id)
      (await (mail coname #f)))


   (define (timestamp) (syscall 201 "%c"))

   (setq RED "\e[0;31m")
   (setq GREEN "\e[0;32m")
   (setq YELLOW "\e[0;33m")
   (setq BLUE "\e[0;34m")
   (setq MAGENTA "\e[0;35m")
   (setq CYAN "\e[0;36m")
   (setq WHITE "\e[0;37m")
   (setq END "\e[0;0m")

   ; ----
   ; todo: use atomic counter to check count of clients and do appropriate timeout on select
   ;  (1 for count > 0, 100 for count = 0)
   (define (on-accept id fd onRequest)
   (delay
      (define (ok l r p v)
         (values l r p v))
      (define (send . args)
         (for-each (lambda (arg)
            (display-to fd arg)) args) #true)

      (print-to stderr) ;; just newline
      (define-values (ss1 ms1) (clock))
      (print-to stderr id "> " (timestamp) " on-accept for " (syscall 51 fd))

      (let ((stream (port->bytestream fd)))
         (with-exception-handler
            (lambda (exception)
               (print-to stderr id "> " RED exception END)
               (send "HTTP/1.0 500 Internal Server Error" "\r\n"
                     "Conneciton: close"                  "\r\n"  ;; todo: keep-alive
                     "\r\n" "500"))
            (lambda ()
               (let*((request stream
                           (let* ((l r p val (http-parser #null stream 0 ok)))
                              (if (not l)
                                 (values #false r)
                                 (values val r)))))
                  (when request
                     (let ((Request (ref request 1))
                           (Headers (ref request 2)))
                        (print-to stderr id "> Request: " GREEN Request END)
                        ;(print-to stderr id ": Headers: " Headers)
                        (if (null? Request)
                           (send "HTTP/1.0 400 Bad Request" "\r\n"
                                 "Conneciton: close"        "\r\n"
                                 "\r\n" "⁉")
                           (onRequest fd Request Headers stream))))))))

      (print-to stderr id "> " (if (syscall 3 fd) "socket closed" "can't close socket"))
      (define-values (ss2 ms2) (clock))
      (print-to stderr id "> " (timestamp) " on-accept processed in "  (+ (* (- ss2 ss1) 1000) (- ms2 ms1)) "ms.")))

   (define (http:run port onRequest)
   (call/cc (lambda (return)
      (define socket (syscall 41))

      ; bind
      (unless (syscall 49 socket port)
         (print-to stderr "can't bind to a port " port)
         (return #false))
      (print-to stderr "server binded to " port)


      ; listen
      (unless (syscall 50 socket)
         (print-to stderr "can't listen a socket")
         (return #false))

      (print-to stderr "server listening to 0.0.0.0:" port)
      ; accept loop
      (let loop ()
         (if (syscall 23 socket ; select
               (if (null? (running-threads)) 30000000 1)) ; 30 second if no running threads detected
            (let ((fd (syscall 43 socket))) ; accept
               (async (on-accept (generate-unique-id) fd onRequest)))
            (sleep 0)) ; else just switch context
         (loop)) )))


))
