#!/usr/bin/ol

(import (lib http))
(import (lib sqlite))


; для упрощения работы и т.д,  пусть у нас будет одна большая база даных с множеством таблиц
(define database (make-sqlite3)) ; make new database connection
(print "open: "  (sqlite3-open (c-string "database.sqlite") database))

(define (sqlite:query query . args)
   (let ((statement (make-sqlite3-stmt)))
      (if (less? 0 (sqlite3-prepare-v2 database (c-string query) -1 statement null))
         (runtime-error "error query preparation" query))
      (let loop ((n 1) (args args))
         (if (null? args) #true
            (let ((arg (car args)))
               (cond
                  ((integer? arg)
                     ;todo: if > max-int-value use sqlite3_bind_int64
                     (sqlite3-bind-int    statement n arg))
                  ((rational? arg)
                     (sqlite3-bind-double statement n arg))
                  ((string? arg)
                     (sqlite3-bind-text   statement n arg (size arg) #f))
                  (else
                     (runtime-error "Unsupported parameter type" arg)))
               (loop (+ n 1) (cdr args)))))
      (case (sqlite3-step statement)
         (SQLITE-ROW
            statement)
         (SQLITE-DONE
            (let ((result (sqlite3-last-insert-rowid database)))
               (sqlite3-finalize statement)
               result))
         (else
            (sqlite3-finalize statement)
            (runtime-error "Can't execute SQL statement" #t)))))

(define (sqlite:for-each statement f)
   (let loop ()
      (let ((n (sqlite3_column_count statement)))
      ;(print "n: " n)
      (if (less? 0 n) (begin
         (apply f
            (let subloop ((i (- n 1)) (args '()))
               ;(print "args: " args)
               ;(print "sqlite3_column_type statement i: " (sqlite3_column_type statement i))
               ;(print "i: " i)
               ;(print "?: " (< i 0))
               (if (< i 0) args
                  (subloop (- i 1) (cons
                     (case (sqlite3_column_type statement i)
                        (SQLITE-NULL    #false)
                        (SQLITE-INTEGER (sqlite3_column_int statement i))
                        ;(SQLITE-FLOAT   (sqlite3_column_double statement i))
                        (SQLITE-TEXT    (sqlite3_column_text statement i))
                        (else (runtime-error "Unsupported column type " i)))
                     args)))))
         ;(print "--------------------")
         (case (sqlite3-step statement)
            (SQLITE-ROW
               (loop))
            (SQLITE-DONE
               (sqlite3-finalize statement))
            (else
               (sqlite3-finalize statement)
               (runtime-error "Can't execute SQL statement" #t))))))))


(sqlite:query "CREATE TABLE IF NOT EXISTS users (
   id INTEGER PRIMARY KEY -- comment
)")

(sqlite:query "CREATE TABLE IF NOT EXISTS scrolls (
   id INTEGER PRIMARY KEY
,  title TEXT
)")
(sqlite:query "INSERT INTO scrolls (title) VALUES (?)" "Planet")
(sqlite:query "INSERT INTO scrolls (title) VALUES (?)" "Minerals on Hand")
(sqlite:query "INSERT INTO scrolls (title) VALUES (?)" "Status")

;(print (sqlite:query "CREATE TABLE IF NOT EXISTS T (id INTEGER PRIMARY KEY, text STRING)"))
;(print (sqlite:query "INSERT INTO T (text) VALUES (?)" "one"))
;(print (sqlite:query "INSERT INTO T (text) VALUES (?)" "two"))
;(print (sqlite:query "INSERT INTO T (text) VALUES (?)" "three"))
;(print (sqlite:query "INSERT INTO T (text) VALUES (?)" "four"))
;
;(sqlite:for-each (sqlite:query
;   "SELECT  text  FROM T")
;   (lambda (text)
;      (print "-" text "-")))














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
                        "   <link href='stylesheets/main.css' type='text/css' rel='stylesheet' />"
                        "</head>")
                  (send "<body>"
                        "<header>-menu-menu-menu-menu-menu-</header>"
                        "   <div id='main'>"
                        "      <view>"
                        "         view"
                        "      </view>"
                        "      <info>"
                        "         <scrolls>")
                  (sqlite:for-each (sqlite:query
                     "SELECT title FROM scrolls")
                     (lambda (title)
                        (send "<scroll>" title "</scroll>")
                  ))
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
   )) ; (close #t)))
