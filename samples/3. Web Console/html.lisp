#!/usr/bin/ol

(import (lib http))

; send file to output stream
(define (sendfile fd content-type filename)
   (print "Sending as '" content-type "' " filename)
(let*((path (c-string (str-app "." filename)))
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

(http:run 8080 (lambda (fd request headers send close)
   (print "Request: " request)
   (print "Headers: " headers)

   (cond
      ((string-eq? (ref request 1) "GET")
         (let ((url (ref request 2)))
            (cond
               ((or (string-eq? url "/")
                    (string-eq? url "/index.html")
                    (string-eq? url "/index.htm"))
                  (sendfile fd "text/html" "/index.html")
                  (close #t))

               ((string-eq? url "/olvm.js")
                  (sendfile fd "application/javascript" "/olvm.js")
                  (close #t))

               ; static content
               ((or (string-eq? url "/static/jquery-1.11.3.min.js")
                    (string-eq? url "/static/jquery.terminal-0.8.8.min.js")
                    (string-eq? url "/static/jquery.mousewheel.min.js"))
                  (sendfile fd "application/javascript" url)
                  (close #t))
               ((string-eq? url "/static/jquery.terminal.css")
                  (sendfile fd "text/css" url) ; charset=UTF-8
                  (close #t))
               ((string-eq? url "/repl")
                  (sendfile fd "application/octet-stream" url)
                  (close #t))

               ((or (string-eq? url "/settings.js")
                    (string-eq? url "/library_gl.js")
                    (string-eq? url "/library_xlib.js"))
                  (sendfile fd "application/javascript" (string-append "/static/emscripten" url))
                  (close #t))

               ((or (string-eq? url "/otus/ffi.scm")
                    (string-eq? url "/owl/math.scm")
                    (string-eq? url "/OpenGL/EGL/version-1-1.scm")
                    (string-eq? url "/OpenGL/ES/version-1-1.scm")
                    )
                  (sendfile fd "application/javascript" (string-append "/../../libraries" url))
                  (close #t))

               ((or (string-eq? url "/egl.lisp"))
                  (sendfile fd "application/javascript" url)
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
                  (close #t))))))))
