---
layout: page
title:  Simple HTTP Server
date: 2016-05-05 12:03:10 UTC
categories: ru
---
> Внимание, эта статья находится в процессе создания; ее содержание может (и будет) меняться, пока полностью не удовлетворит автора. А до тех пор я не ручаюсь за стопроцентную достоверность приведенной информации.

#### Простой пример

   Otus Lisp позволяет быстро прототипировать типичные задачи. Например, можно создать простой http сервер всего парой строк:

<pre><code data-language="scheme">
(import (lib http))

(http:run 8080 (lambda (fd request headers send close)
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*)
         "\n\n"
         "<h1>200: OK</h1>"
         (ref request 1) ": " (ref request 2)
         "<hr><small>" headers
         "</small>")
   (close #t)))
</code></pre>

   Этот сервер принимает запросы на порт 8080, и на любой запрос отдает пустую страничку с надписью 200: OK и списком переданных клиентом заголовков.

#### Пример посложнее

   В этом примере мы сделаем "настоящий" веб-сервер. То есть такой, который умеет отдавать нам контент, правильно заполняя поле content-type.

   Создадим функцию sendfile, которая проверяет наличие файла, отправляя при его отсутствии код 404.

<pre><code data-language="scheme">
(import (lib http))

; send file to output stream
(define (sendfile fd content-type filename)
   (print "Sending " filename " as '" content-type "'")
(let*((path (if (string? filename) (str-app "." (c-string filename)) "?"))
      (send (lambda args
         (for-each (lambda (arg)
            (display-to fd arg)) args)))
      (stat (syscall 4 path #f #f)))
   (if stat (begin
      (print "Sending 200 OK, file size is " (ref stat 8) ", name is " path)
      (send "HTTP/1.0 200 OK\n"
            "Connection: close\n"
            "Content-Type: " content-type "\n"
            "Content-Length: " (ref stat 8) "\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
      (write-vector (file->vector path) fd)
      (print "File sent."))
   ;else
   (begin
      (print "Sending 404 Not Found")
      (send "HTTP/1.0 404 Not Found\n")
      (send "Connection: close\n"
            "Content-Type: text/html\n"
            "Server: " (car *version*) "/" (cdr *version*) "\n\n")
      (send "<h1>404 Not Found OK</h1>"
            "<h4>url: " filename "</h4>")))))

(define has-two-dots? (string->regex "m/\\.\\./"))

(define (starts-with string sub)
   (if (> (string-length sub) (string-length string))
      #false
      (string-eq? (substring string 0 (string-length sub)) sub)))

(http:run 8080 (lambda (fd request headers send close)
   (cond
      ((string-eq?  (ref request 1) "GET")
         (let ((url (ref request 2)))
            (cond
               ;basic sanity check:
               ((has-two-dots? url)
                  (sendfile fd "text/html" 404))

               ;index
               ((or
                  (string-eq? url "/")
                  (starts-with url "/index.htm"))
                     (sendfile fd "text/html" "/index.html"))

               ((starts-with url "/images/")
                  (sendfile fd "image/png" url))

               ((starts-with url "/static/")
                  (sendfile fd "image/html" url))

               (else
                  (sendfile fd "text/html" 404)))))
      (else
          (sendfile fd "text/html" 404)))
   (close #t)))
</code></pre>

   Основной код обработки http запросов находится в библиотеке lib/http.owl, которая доступна в поставке Ol или в официальном репозитории.
