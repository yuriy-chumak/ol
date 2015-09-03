; http server
(define-library (lib http)
  (export
    http:run)
  (import (r5rs base) (owl parse) (owl math) (owl list) (owl io) (owl string) (owl ff) (owl list-extra) (owl error) (owl interop))
  
(begin
  ;(define *debug* #t)
  ;(print "\e[1;1H\e[2J")

(define (timestamp)
   (let* ((ss ms (clock)))
      (list (mod (floor (/ ss 60)) 24)
            (mod ss 60)
            ms)))
(define (set-ticker-value n) (sys-prim 1022 n #false #false))


(define (send fd . args)
   (for-each (lambda (arg)
      (display-to fd arg)) args))


; parser
      (define (syntax-fail pos info lst)
         (list "syntax-error-mark" info
            (list ">>> " pos "-" (runes->string lst) " <<<")))

      (define quoted-values
         (list->ff
            '((#\a . #x0007)
              (#\b . #x0008)
              (#\t . #x0009)
              (#\n . #x000a)
              (#\r . #x000d)
              (#\e . #x001B)
              (#\" . #x0022)
              (#\\ . #x005c))))
              
      (define (a-z? x)
         (<= #\a x #\z))
      (define (A-Z? x)
         (<= #\A x #\Z))
      (define (uri? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\Z)
             (<= #\a x #\z)
             (has? '(#\/ #\: #\.) x)))

      (define (space-char? x) (= x #\space))

      (define (digit-char? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\F)
             (<= #\a x #\f)))
      (define (alpha-char? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\Z)
             (<= #\a x #\z)))
      (define (url-char? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\Z)
             (<= #\a x #\z)))
             
      (define (header-char? x)
         (or (<= #\A x #\Z)
             (<= #\a x #\z)
             (has? '(#\-) x)))

      (define digit-values
         (list->ff
            (foldr append null
               (list
                  (map (lambda (d) (cons d (- d 48))) (iota #\0 1 #\9))  ;; 0-9
                  (map (lambda (d) (cons d (- d 55))) (iota 65 1 71))  ;; A-F
                  (map (lambda (d) (cons d (- d 87))) (iota 97 1 103)) ;; a-f
                  ))))
      (define (bytes->number digits base)
         (fold
            (λ (n digit)
               (let ((d (get digit-values digit #false)))
                  (cond
                     ((or (not d) (>= d base))
                        (error "bad digit " digit))
                     (else
                        (+ (* n base) d)))))
            0 digits))


      (define get-rest-of-line
         (let-parses
            ((chars (get-greedy* (get-byte-if (lambda (x) (not (eq? x #\newline))))))
             (skip (get-imm #\newline))) ;; <- note that this won't match if line ends to eof
            chars))
      (define get-a-whitespace
         (get-any-of
            ;get-hashbang   ;; actually probably better to make it a symbol as above
            (get-byte-if (lambda (x) (has? '(#\space #\tab #\newline #\return) x)))
            (let-parses
               ((skip (get-imm #\;))
                (skip get-rest-of-line))
               'comment)))
      (define maybe-whitespace (get-kleene* get-a-whitespace))

      (define get-request-line
         (let-parses (
             (method (get-greedy+ (get-rune-if A-Z?)))
             (skip        (get-imm #\space))
             (uri    (get-greedy+ (get-rune-if uri?)))
             (skip        (get-imm #\space))
             (proto  (get-greedy+ (get-rune-if uri?))) ; todo:
             (skip (get-imm #\return)) (skip (get-imm #\newline)))
            (begin
               (print (timestamp) "> request uri: " (runes->string uri))
            (list
               (runes->string method)
               (runes->string uri)
               (runes->string proto)))))

      (define get-header-line
         (let-parses (
             (name (get-greedy+ (get-rune-if header-char?)))
             (skip (get-imm #\:))
             (skip (get-byte-if space-char?))
             (value (get-greedy+ (get-rune-if (lambda (x) (not (eq? x #\return))))))
             (skip (get-imm #\return)) (skip (get-imm #\newline)))
            (cons
               (runes->string name)
               (runes->string value))))


      (define http-parser
         (let-parses (
             (request-line get-request-line)
             (headers-array (get-greedy* get-header-line))
             (skip (get-imm #\return)) (skip (get-imm #\newline)))
            (list
               request-line headers-array)))


;(print (car
;(file->exp-stream "GET" "> " http-parser syntax-fail)))
;(exit-owl 0)

   
;   (exit-owl (print "Can't bind to 8080")))


; todo: use atomic counter to check count of clients and do appropriate timeout on select
;  (1 for count > 0, 100 for count = 0)

(define (on-accept fd onRequest)
(lambda ()
   (let* ((ss ms (clock)))
   (print "connected at " (timestamp))
   (let ((send (lambda args
            (for-each (lambda (arg)
               (display-to fd arg)) args))))
   (let ((request (fd->exp-stream fd "> " http-parser syntax-fail #f)))
      (print "request is " (car request))
      (send "HTTP/1.1 200 OK\nContent-Type: text/html; charset=UTF-8\n\n<HTML><BODY>")
      (onRequest (car request) send)
      (send "<HR>" (let* ((s2 m2 (clock))) (+ (* (- s2 ss) 1000) (- m2 ms))) "ms " "</BODY></HTML>"))
   (syscall 3 fd #f #f)
   (print "done." )))))


(define (http:run port onRequest)
(let ((socket (syscall 41 #f #f #f)))
   ; bind
   (let loop ((port port))
      (if (not (syscall 49 socket port #f))
         (loop (+ port 2))
         (print "Server binded to " port)))
   ; listen      
   (if (not (syscall 50 socket #f #f))   (exit-owl (print "Can't listen")))
   
   ; accept
   (let loop ((socket socket))
      (if (syscall 23 socket #f #f)
         (let ((fd (syscall 43 socket #f #f)))
            (fork (on-accept fd onRequest)))
         (set-ticker-value 0))
      (loop socket))))
))