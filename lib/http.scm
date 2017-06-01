; http server
; HTTP/1.0 - https://tools.ietf.org/html/rfc1945
(define-library (lib http)
  (export
    http:run)
  (import (r5rs core) (owl parse)
      (owl math) (owl list) (owl io) (owl string) (owl ff) (owl list-extra) (owl interop)
      (only (lang intern) string->symbol)
      (only (lang sexp) fd->exp-stream)
     )

(begin
   (define (upper-case? x) (<= #\A x #\Z))
   (define (lower-case? x) (<= #\a x #\z))
   (define (alpha-char? x)
      (or (upper-case? x)
          (lower-case? x)))
   (define (digit-char? x) (<= #\0 x #\9))
   (define (ctl-char? x)
      (or (<= 0 x 31)
          (= x 127)))

; unique session id:
(fork-server 'IDs (lambda ()
(let this ((id 1))
(let* ((envelope (wait-mail))
       (sender msg envelope))
   (mail sender id)
   (this (+ id 1))))))
(define (generate-unique-id)
   (interact 'IDs #f))



(define (timestamp) (syscall 201 "%c" #f #f))
(define (set-ticker-value n) (syscall 1022 n #false #false))

; parser
      (define (syntax-fail pos info lst)
         (print "http parser fail: " info)
         (print ">>> " pos "-" (runes->string lst) " <<<")
         '(() (())))

      (define quoted-values
         (list->ff
            '((#\a . #x0007)
              (#\b . #x0008)
              (#\t . #x0009)
              (#\n . #x000a)
              (#\r . #x000d)
              (#\e . #x001B)
              (#\" . #x0022) ;"
              (#\\ . #x005c))))

      (define (a-z? x)
         (<= #\a x #\z))
      (define (A-Z? x)
         (<= #\A x #\Z))
      (define (uri? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\Z)
             (<= #\a x #\z)
             (has? '(#\/ #\: #\. #\& #\? #\- #\+ #\= #\< #\> #\@ #\# #\_ #\% #\, #\; #\' #\!) x)))
      (define (xml? x)
         (or (<= #\0 x #\9)
             (<= #\A x #\Z)
             (<= #\a x #\z)
             (has? '(#\< #\/ #\> #\-) x)))


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

      ;(define (separator-char?))

      (define digit-values
         (list->ff
            (foldr append null
               (list
                  (map (lambda (d) (cons d (- d 48))) (lrange #\0 1 #\9))  ;; 0-9
                  (map (lambda (d) (cons d (- d 55))) (lrange 65 1 71))  ;; A-F
                  (map (lambda (d) (cons d (- d 87))) (lrange 97 1 103)) ;; a-f
                  ))))
      (define (bytes->number digits base)
         (fold
            (λ (n digit)
               (let ((d (get digit-values digit #false)))
                  (cond
                     ((or (not d) (>= d base))
                        (runtime-error "bad digit " digit))
                     (else
                        (+ (* n base) d)))))
            0 digits))

;               ((head (get-rune-if symbol-lead-char?))
;                (tail (get-greedy* (get-rune-if symbol-char?)))
;                (next (peek get-byte))
;                (foo (assert (lambda (b) (not (symbol-char? b))) next))) ; avoid useless backtracking
;               (string->uninterned-symbol (runes->string (cons head tail))))

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
            (tuple
               (runes->string method)
               (runes->string uri)
               (runes->string proto))))

      (define get-header-line
         (let-parses (
             (name (get-greedy+ (get-rune-if header-char?)))
             (skip (get-imm #\:))
             (skip (get-byte-if space-char?))
             (value (get-greedy+ (get-rune-if (lambda (x) (not (eq? x #\return))))))
             (skip (get-imm #\return)) (skip (get-imm #\newline)))
            (cons
               (string->symbol (runes->string name))
;               (string->uninterned-symbol (runes->string name))
               (runes->string value))))


      (define http-parser
;         (get-any-of       ;process "GET /smth HTTP/1.1"
            (let-parses (
                  (request-line get-request-line)
                  (headers-array (get-greedy* get-header-line))
                  (skip (get-imm #\return)) (skip (get-imm #\newline))) ; final \r\n
               (cons
                  request-line (list->ff headers-array)))
;            (let-parses ( ; process '<policy-file-request/>\0' request:
;                  (request (get-greedy+ (get-rune-if xml?))))
;;                  (skip    (get-imm 0)))
;               (cons
;                  (tuple (runes->string request)) #empty))
;                  )
)


; todo: use atomic counter to check count of clients and do appropriate timeout on select
;  (1 for count > 0, 100 for count = 0)

(define (on-accept id fd onRequest)
(lambda ()
   (let*((ss1 ms1 (clock))
         (send (lambda args
                  (for-each (lambda (arg)
                     (display-to fd arg)) args) #t)))

      (let loop ((request (fd->exp-stream fd "" http-parser syntax-fail #f)))
         (print id " loop:" request)
         (if (call/cc (lambda (close)
                         (if (null? request)
                            #t
                            (let ((Request-Line (car (car request)))
                                  (Headers-Line (cdr (car request))))
                               ;(print "Request-Line: " Request-Line)
                               ;(print "Headers-Line: " Headers-Line)
                               (if (null? Request-Line)
                                  (close (send "HTTP/1.0 400 Bad Request\r\n\r\n400"))
                                  (onRequest fd Request-Line Headers-Line send close))
                            (print "ok.")
                            #f)) #|(close #t)|# ))
               
            (begin
               (display id)
               (display (if (syscall 3 fd #f #f) " socket closed, " " can't close socket, ")))
            ;else
            (loop (force (cdr request)))))
      (print "on-accept done." )
      (let*((ss2 ms2 (clock)))
         (print id " # " (timestamp) ": request processed in "  (+ (* (- ss2 ss1) 1000) (- ms2 ms1)) "ms.")))

   (let sleep ((x 1000))
      (set-ticker-value 0)
      (if (> x 0)
         (sleep (- x 1))))
   ))


(define (http:run port onRequest)
(let ((socket (syscall 41 #f #f #f)))
   ; bind
   (let loop ((port port))
      (if (not (syscall 49 socket port #f)) ; bind
         (loop (+ port 2))
         (print "Server binded to " port)))
   ; listen
   (if (not (syscall 50 socket #f #f)) ; listen
      (exit-owl (print "Can't listen")))

   ; accept
   (let loop ()
      (if (syscall 23 socket #f #f) ; select
         (let ((fd (syscall 43 socket #f #f))) ; accept
            (print "\n# " (timestamp) ": new request from " (syscall 51 fd #f #f))
            (fork (on-accept (generate-unique-id) fd onRequest))))
      (set-ticker-value 0)
      (loop))))
))