#!/usr/bin/ol

(import (lib http))

,load "sqlite.lisp"




(define has-two-dots? (string->regex "m/\\.\\./"))

; syscalls
(define (yield) (syscall 1022 0 #false #false))
(define (time format seconds) (syscall 201 format seconds #f))
(define uname (syscall 63 0 0 0))

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

;(define (exec filename args fds)
;   (syscall 59 (c-string filename)
;      (map c-string args) fds))
(define (concat . args)
   (foldr str-app "" args))

; ==============================================================
; todo: please,  return sendfile

; todo: 401 Unauthorized
; todo: 403 Forbidden
(define (send-404 fd)
(let*((send (lambda args
         (for-each (lambda (arg)
            (display-to fd arg)) args))))
   (print "Sending error")
   (print "Sending 404 Not Found")

   (send "HTTP/1.0 404 Not Found\n")
   (send "Connection: close\n"
         "Content-Type: text/html\n"
         "Server: " (car *version*) "/" (cdr *version*) "\n\n")
   (send "<HTML><BODY>"
         "<h1>404 Not Found OK</h1>")))

(define (sendfile fd content-type filename)
   (print "Sending as '" content-type "' " filename)
(let*((path (if (string? filename) (str-app "." (c-string filename)) "?"))
      (send (lambda args
         (for-each (lambda (arg)
            (display-to fd arg)) args)))
      (stat (syscall 4 path #f #f)))
   (if stat (begin
      (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)
      (send "HTTP/1.0 200 OK\n"
            ;"Connection: close\n"
            "Content-Type: " content-type "\n"
            "Content-Length: " (ref stat 8) "\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
      (write-vector (file->vector path) fd)
      (print "File sent."))
   ;else
   (begin
      (print "Sending 404 Not Found")
      (send "HTTP/1.0 404 Not Found\n"
            ;"Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
      (send "<HTML><BODY>"
            "<h1>404 Not Found OK</h1>"
            "<h4>url: " filename "</h4>")))))



(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
   (print "Headers: " headers)

   (cond
      ((string-eq? (ref request 1) "GET")
         (let ((url (ref request 2)))
            (cond
               ; basic sanity check:
               ((has-two-dots? url)
                  (send-404 fd))

               ; static web content:
               ((starts-with url "/javascripts/")
                  (sendfile fd "application/javascript" url))
               ((starts-with url "/stylesheets/")
                  (sendfile fd "text/css" url))

               ; dynamic functions
               ((or
                  (string-eq? url "/")
                  (string-eq? url "/index.html"))

                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")
                  (send "<!DOCTYPE html>"
                        "<html>"
                        "<head>"
                        "   <title>Some title</title>"
                        "   <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />"
                        "   <link href='/stylesheets/main.css' type='text/css' rel='stylesheet' />"
                        "   <script src='/javascripts/jquery-2.1.1.min.js'></script>"
                        "</head>"
                        "<body>"
                        "   <div id='main'>"
                        "   </div>"
                        ; и сразу перейдем к списку игр
                        "   <script>$('#main').load('/games')</script>"
                        "</body>"
                        "</html>"))
               ; список игр пользователя
               ((string-eq? url "/games")
                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")
                  (let ((account 0))
                  (sqlite:for-each (sqlite:query
                     "SELECT  id,name  FROM games WHERE id IN (
                         SELECT DISTINCT game FROM game_players WHERE race IN (
                            SELECT id FROM races WHERE account = ?
                         )
                      )" account)
                     (lambda (id name)
                        (send "<br><a href='/game/" id "'>" id " " name "</a>")
                     )))
                  (send "<br><hr>"))
               ((starts-with url "/game/")
                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")
                  (send "<!DOCTYPE html>"
                        "<html>"
                        "<head>"
                        "   <title>Some title</title>"
                        "   <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />"
                        "   <link href='/stylesheets/main.css' type='text/css' rel='stylesheet' />"
                        "   <script src='/javascripts/jquery-2.1.1.min.js'></script>"
                        "</head>"
                        "<body>"
                        "   <header>"
                        url
                        "   </header>"
                        "   <div id='main'>"
                        "      <view>"
                        "         view"
                        "      </view>"
                        "      <info>"
                        "         <scrolls>")
                  ;(sqlite:for-each (sqlite:query
                  ;   "SELECT title FROM scrolls")
                  ;   (lambda (title)
                  ;      (send "<scroll>" title "</scroll>")
                  ;))
                  (send "         </scrolls>"
                        "         <mailbox>Mailbox</mailbox>"
                        "         <mailbox>Summary</mailbox>"
                        "      </info>"
                        "   </div>"
                        "</body>"
                        "</html>"))



               ;else
               (else
                  (send-404 fd)))))
      (else
         (send-404 fd)))
   (close #t)))
