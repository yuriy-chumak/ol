---
layout: page
title:  "Примеры"
date:   2015-11-27 18:54:46
categories: ru
---
#### Простой веб-сервер
         
   Слушает порт 8080, на любой корректный запрос (GET, POST, etc.) возвращает "200 OK" и страничку с эхом заголовков из запроса.

<pre><button class="doit" onclick="function(){return false}">Этот пример в терминале работать не будет</button>
<code data-language="scheme">#!/bin/ol

(import (lib http))
; simplest example:
(http:run 8080 (lambda (fd request headers send close)
   (send "HTTP/1.0 200 OK\n"
         "Connection: close\n"
         "Content-Type: text/html; charset=UTF-8\n"
         "Server: " (car *version*) "/" (cdr *version*) "\n\n"

         "<h1>200: OK</h1>"
         (ref request 1) ": " (ref request 2)
         "<hr><small>" headers
         "</small>")
   (close #t)
))
</code></pre>

#### Числомолотилка

   Простая, но интересная задачка о сумме собственных делителей числа. Печатает цикл суммы собственных делителей, пока не встретит начальное число. Детальнее можно узнать по ссылке из комментария в коде.

<pre><button class="doit" onclick="doit(numbers.textContent)">отправить в терминал</button>
<code data-language="scheme" id="numbers">; http://math.d3.ru/comments/591298/
(define (factors n)
   (let *factors ((d 1))
      (cond ((> d n) (list))
            ((= (mod n d) 0) (cons d (*factors (+ d 1))))
            (else (*factors (+ d 1))))))

(define (sum-factors n)
      (let ((f (factors n)))
        (let ((s (- (apply + f) n)))
           (print n ": " f " > " s)
         s)))

(define (go-throw-factors-cycle n)
  (let *go ((x n))
      (let ((s (sum-factors x)))
        (if (or (= s n) (= s 0) (= s x)) s (*go s)))))

(go-throw-factors-cycle 14316)
</code></pre>

   Some works more.
</span>
