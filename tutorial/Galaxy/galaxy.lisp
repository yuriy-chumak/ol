#!/usr/bin/ol

(import (lib http))

,load "sqlite.lisp"
,load "helpers.lisp"



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
;   (let send-200 ()
;      (send "HTTP/1.0 200 OK\n"
;            "Content-Type: text/html\n"
;            "Server: " (car *version*) "/" (cdr *version*) "\n"
;            "\n")

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
                  (string-eq? url "/index.html")
                  (string-eq? url "/login.html"))

                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")
;                  (send "<!DOCTYPE html>"
;                        "<html>"
;                        "<head>"
;                        "   <title>Some title</title>"
;                        "   <meta http-equiv='Content-Type' content='text/html; charset=utf-8' />"
;;                        "   <link href='/stylesheets/main.css' type='text/css' rel='stylesheet' />"
;                        "   <script src='/javascripts/jquery-2.1.1.min.js'></script>"
;                        "</head>"
;                        "<body>"
;                        "   <div id='main'>"
;                        "   </div>"
;                        ; и сразу перейдем к списку игр
;                        "   <script>$('#main').load('/games')</script>"
;                        "   <noscript><b>Looks like you have disabled scripts for this page. Please enable it to continue working with.</b></noscript>"
;                        "</body>"
;                        "</html>"))
                  (send "<!DOCTYPE html>"
                        "<html>"
                        "<head>"
                        "   <title>SIMPLE Login</title>"
                        "   <link rel='stylesheet' href='/stylesheets/normalize.css'>"
                        "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>"
                        "   <link rel='stylesheet' href='/stylesheets/login.css'>"
                        "   <script src='/javascripts/jlogin.js'></script>"
                        "</head>"
                        "<body>"
                        "   <section class='loginform cf'>"
                        "      <form name='login' action='/login' method='get' accept-charset='utf-8' onsubmit='return onSubmit()'>"
                        "         <ul>"
                        "            <li>"
                        "               <label for='usermail'>Email</label>"
                        "               <input type='usermail' id='usermail' placeholder='yourname@email.com' required></li>"
                        "            <li>"
                        "               <label for='password'>Password</label>"
                        "               <input type='password' id='password' placeholder='password' required></li>"
                        "            <li>"
                        "               <input type='submit' value='Login'>"
                        "            </li>"
                        "         </ul>"
                        "      </form>"
                        "   </section>"
                        "</body>"
                        "</html>"))
               ; обработка логина пользователя
               ((starts-with url "/login/") ;e7bf1d8e30d58da23c37d9eef9da55cb
                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")

                  (let ((args (string-split url #\/)))
                  (let ((username (list-ref args 1))
                        (password (list-ref args 2)))
                  (send "<!DOCTYPE html>"
                        "<html>"
                        "<head>"
                        "   <title>SIMPLE Login</title>"
                        "   <link rel='stylesheet' href='/stylesheets/normalize.css'>"
                        "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>"
                        "   <link rel='stylesheet' href='/stylesheets/login.css'>"
                        "</head>")
                  (let ((key (sqlite:value "SELECT lower(hex(randomblob(16)))")))
                  (if (sqlite:value "UPDATE accounts SET key=? WHERE username=? AND password=?" key username password)
                  ; ok
                  (send "<body>"
                        "   <script>$('#main').load('/games/" key "')</script>"
                        "   <noscript><b>Looks like you have disabled scripts for this page. Please enable it to continue working with.</b></noscript>"
                        "</body>")
                  ; false
                  (send "<body>"
                        "   <section class='loginform cf'>"
                        "      <form name='sorry' action='/' method='get' accept-charset='utf-8' onsubmit='window.location = \"/\"; return false'>"
                        "         <ul>"
                        "            <li>"
                        "               <label>Sorry, name or password is invalid.</label>"
                        "            </li>"
                        "            <li>"
                        "               <label>Please, <a href='/'> try again</a>.</label>"
                        "            </li>"
                        "         </ul>"
                        "      </form>"
                        "   </section>"
                        "</body>")))
                  (send "</html>"))))

               ; список игр пользователя
               ((string-eq? url "/games/")
                  (send "HTTP/1.0 200 OK\n"
                        "Content-Type: text/html\n"
                        "Server: " (car *version*) "/" (cdr *version*) "\n"
                        "\n")
                  (let ((args (string-split url #\/)))
                  (let ((key (list-ref args 1)))

                  (let ((account (sqlite:value "SELECT id FROM accounts WHERE key = ?" key)))
                  (if account (begin
                     (sqlite:for-each (sqlite:query
                        "SELECT  id,name  FROM games WHERE id IN (
                            SELECT DISTINCT game FROM game_players WHERE race IN (
                               SELECT id FROM races WHERE account = ?
                            )
                         )" account)
                        (lambda (id name)
                           (send "<br><a href='/game/" account "/" id "'>" id " " name "</a>")
                        )))
                     ;else
                     (send "I don't know who you are!"))
                  (send "<br><hr>")))))

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
