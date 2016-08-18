#!/usr/bin/ol

(import (lib http))

,load "sqlite.lisp"
,load "helpers.lisp"

(define (ne? . args)
   (not (apply eq? args)))


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


; ===================================================
(http:run 8080 (lambda (fd request headers send close)
   (define (send-200 title . extra)
      (send "HTTP/1.0 200 OK\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n"
            "\n")
      (send "<!DOCTYPE html>"
            "<html>"
            "<head>"
            "   <title>" title "</title>"
            "   <link rel='stylesheet' href='/stylesheets/normalize.css'>"
            "   <script src='/javascripts/jquery-2.1.1.min.js'> </script>")
      (for-each send extra)
      (send "</head><body>")
      #true)
   (define (send-end)
      (send "</body></html>"))
   (define (send-400)
      (send "HTTP/1.0 400 Bad Request\n")
      (send "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n"
            "\n")
      (send "<html><body>"
            "<h1>400 Bad Request</h1>")
      #true)
   (define (send-401)
      (send "HTTP/1.0 401 Unauthorized\n")
      (send "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n"
            "\n")
      (send "<html><body>"
            "<h1>401 Unauthorized</h1>")
      #true)
   (define (send-404)
      (send "HTTP/1.0 404 Not Found\n")
      (send "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n"
            "\n")
      (send "<html><body>"
            "<h1>404 Not Found</h1>")
      #true)


   (print "Request: " request)
   (print "Headers: " headers)

   (cond
      ((string-eq? (ref request 1) "GET")
         (let ((url (ref request 2)))
            (cond
               ; basic sanity check:
               ((has-two-dots? url)
                  (send-404))

               ; static web content:
               ((starts-with url "/javascripts/")
                  (sendfile fd "application/javascript" url))
               ((starts-with url "/stylesheets/")
                  (sendfile fd "text/css" url))

               ; dynamic functions
               ((or
                  (string-eq? url "/")
                  (string-eq? url "/index.html"))
                  ; todo: change to 303 See Other to /login.html

                  (send-200 "SIMPLE Login"
                        "   <link rel='stylesheet' href='/stylesheets/login.css'>"
                        "   <script src='/javascripts/jlogin.js'></script>")
                  (send "   <section class='loginform cf'>"
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
                        "   </section>")
                  (send-end))
               ; обработка логина пользователя
               ((starts-with url "/login/") ; usermail/password
                  (let ((args (string-split url #\/)))

                  (if (ne? (length args) 3)
                     (close (send-400)))
                  ; else - ok
                  (let ((username (list-ref args 1))
                        (password (list-ref args 2)))
                     (send-200 "Logging in..."
                           "   <link rel='stylesheet' href='/stylesheets/login.css'>")
                     ; сгенерируем сеансовый ключ
                     (let ((session (sqlite:value "SELECT lower(hex(randomblob(16)))")))                                         ; remote peer address
                        (if (sqlite:value "UPDATE accounts SET session=?, address=? WHERE username=? AND password=?" session (car (syscall 51 fd #f #f)) username password)
                     ; ok
                     (send "   <script>window.location.href = '/games/" session "'</script>")
                     ; false
                     (send "   <section class='loginform cf'>"
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
                           "   </section>")))
                     (send-end))))

               ; список игр пользователя
               ((starts-with url "/games/") ; session
                  (let ((args (string-split url #\/)))

                  (if (ne? (length args) 2)
                     (close (send-400)))

                  (let ((session (list-ref args 1)))
                  (let ((account (sqlite:value "SELECT id FROM accounts WHERE session = ? AND address = ?" session (car (syscall 51 fd #f #f)))))
                     (if (not account)
                        (close (send-401)))

                     ; ok, все проверки закончены
                     (send-200 "List")

                     (sqlite:for-each (sqlite:query
                        "SELECT  id,name  FROM games WHERE id IN (
                            SELECT DISTINCT game FROM game_players WHERE race IN (
                               SELECT id FROM races WHERE account = ?
                            )
                         )" account)
                        (lambda (id name)
                           (send "<br><a href='/game/" session "/" id "'>" id " / " name "</a>")
                        ))

                     (send "<br><hr>")
                     (send-end)))))

               ((starts-with url "/game/") ; session/game
                  (let ((args (string-split url #\/)))

                  (if (ne? (length args) 3)
                     (close (send-400)))

                  (let ((session (list-ref args 1))
                        (game    (list-ref args 2)))
                  (let ((account (sqlite:value "SELECT id FROM accounts WHERE session = ? AND address = ?" session (car (syscall 51 fd #f #f)))))
                     (if (not account)
                        (close (send-401)))
                     ; todo: добавить проверку на то, что игра действительно принадлежит игроку
                     ;       и вообще,  все проверки стоит впихнуть в один большой запрос

                     ; ok
                     (send-200 "One"
                           "   <link href='/stylesheets/main.css' type='text/css' rel='stylesheet' />")
                     (send "   <header>"
                           (sqlite:value "SELECT name FROM games WHERE id=?" game)
                           "   </header>"
                           "   <div id='main'>"
                           "      <view>"
                           "         view"
                           "      </view>"
                           "      <info>"
                           "         <scrolls>")
                     (send "<scroll>" "title1" "</scroll>")
                     (send "<scroll>" "title2" "</scroll>")
                     (send "<scroll>" "title3" "</scroll>")
                     (send "         </scrolls>"
                           "         <mailbox>Mailbox</mailbox>"
                           "         <mailbox>Summary</mailbox>"
                           "      </info>"
                           "   </div>")
                     (send-end)))))



               ;else
               (else
                  (send-404 fd)))))
      (else
         (send-404 fd)))
   (close #t)))
