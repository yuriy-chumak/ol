; http server
; HTTP/1.0 - https://tools.ietf.org/html/rfc1945
(define-library (lib http)
  (export
    http:run
    http:parse-url)
  (import (scheme base) (scheme srfi-1)
      (owl parse) (otus blobs)
      (owl math) (owl list) (owl io) (owl string) (owl ff) (owl list-extra) (owl interop)
      (only (lang intern) string->symbol))

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

; -------------------------------------------
; http parser

(define HT #\tab)      ; 9
(define CTL (append (iota 32) '(127))) ; 0 .. 31, 127
(define CR #\return)   ; 13
(define LF #\newline)  ; 10
(define SP #\space)    ; 32
(define tspecials '(#\( #\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\/ #\[ #\] #\? #\= #\{ #\} #\space #\tab))

(define (token-char? x)
   (and (less? 31 x) (not (eq? 127 x))    ; not CTL
         (not (has? tspecials x))))

(define (a-z? x)
   (<= #\a x #\z))
(define (A-Z? x)
   (<= #\A x #\Z))
(define (uri? x)
   (or (<= #\a x #\z)
         (<= #\A x #\Z)
         (<= #\0 x #\9)
         (has? '(#\- #\. #\_ #\~ #\: #\/ #\? #\# #\@ #\! #\$ #\& #\' #\( #\) #\* #\+ #\, #\; #\= #\. #\%) x)
         (has? '(#\{ #\} #\| #\\ #\^ #\[ #\] #\`) x))) ;unwise characters are allowed but may cause problems
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

(define (header-char? x)
   (or (<= #\A x #\Z)
         (<= #\a x #\z)
         (has? '(#\-) x)))

;(define (separator-char?))

(define digit-values
   (list->ff
      (foldr append null
         (list
            (map (lambda (d) (cons d (- d 48))) (lrange 48 1 58))  ;; 0-9
            (map (lambda (d) (cons d (- d 55))) (lrange 65 1 71))  ;; A-F
            (map (lambda (d) (cons d (- d 87))) (lrange 97 1 103)) ;; a-f
            ))))

(define hex-table
   (list->ff (fold append '() (list
      (zip cons (iota 10 #\0) (iota 10 0))     ; 0-9
      (zip cons (iota  6 #\a) (iota 6 10))     ; a-f
      (zip cons (iota  6 #\A) (iota 6 10)))))) ; A-F


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
   (let-parses ((method  (get-greedy+ (get-rune-if A-Z?))) ; GET/POST/etc.
                  (-          (get-imm #\space))
                  (uri     (get-greedy+ (get-rune-if uri?))) ; URI
                  (-          (get-imm #\space))
                  (version (get-greedy+ (get-rune-if uri?))) ; HTTP/x.x
                  (-          (get-imm #\return)) (skip (get-imm #\newline))) ; end of request line
      (tuple
         (runes->string method)
         (runes->string uri)
         (runes->string version))))

(define get-header-line
   (let-parses ((name  (get-greedy+ (get-rune-if token-char?)))
                  (-        (get-imm #\:))
                  (-        (get-rune-if space-char?)) ; todo: should be LWS
                  (value (get-greedy+ (get-rune-if (lambda (x) (not (eq? x CR)))))) ; until CRLF
                  (- (get-imm CR)) (- (get-imm LF)))   ; CRLF
      (cons
         (string->symbol (runes->string name))
         (runes->string value))))


(define http-parser
   (let-parses ((request-line get-request-line)
                  (headers-array (get-greedy* get-header-line))
                  (- (get-imm CR)) (- (get-imm LF)))   ; final CRLF
      (tuple
         request-line (list->ff headers-array))))
;)

; ----
; todo: use atomic counter to check count of clients and do appropriate timeout on select
;  (1 for count > 0, 100 for count = 0)
(define (on-accept id fd onRequest)
(lambda ()
   (define (ok data fail val pos)
      ;(print-to stderr "request: " val)
      ;(print-to stderr "data: " data)
      (values val data))
   (define (fail pos reason)
      (print-to stderr "http parser fail: \e[0;31m" reason "\e[0;0m")
      (values #false #null))
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args) #t)

   (print "on-accept :" id)
   (let*((ss1 ms1 (clock)))
      (let loop ((stream (port->byte-stream fd)))
         ;(print "stream: " stream)
         (let* ((request stream (http-parser stream ok fail 0)))
            (if request
               (unless (call/cc (lambda (close)
                              (let ((Request-Line (ref request 1))
                                    (Headers-Line (ref request 2)))
                                 (print id ": Request-Line: \e[0;34m" Request-Line "\e[0;0m")
                                 (print id ": Headers-Line: " Headers-Line)
                                 (if (null? Request-Line)
                                    (close (send "HTTP/1.0 400 Bad Request\r\n\r\n400"))
                                    (onRequest fd Request-Line Headers-Line send close))
                                 (print "ok."))
                              #false))
                  (loop stream)))))
      (print id (if (syscall 3 fd #f #f) ": socket closed" ": can't close socket"))
      (print "on-accept :" id " done.")
      (let*((ss2 ms2 (clock)))
         (print "# " (timestamp) ": request " id " processed in "  (+ (* (- ss2 ss1) 1000) (- ms2 ms1)) "ms.")))
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
      (shutdown (print "Can't listen")))

   ; accept
   (let loop ()
      (if (syscall 23 socket #f #f) ; select
         (let ((fd (syscall 43 socket #f #f))) ; accept
            (print "\n# " (timestamp) ": new request from " (syscall 51 fd #f #f))
            (fork (on-accept (generate-unique-id) fd onRequest))))
      (set-ticker-value 0)
      (loop))))

; -=( parse url )=-------------------------------------
(define (get-path url)
(let loop ((u url) (path #null))
   (cond
      ((null? u)
         (values (bytes->string (reverse path)) u))
      ((eq? (car u) #\?)
         (values (bytes->string (reverse path)) (cdr u)))
      (else
         (loop (cdr u) (cons (car u) path))))))

(define (get-key u)
(let loop ((u u) (key #null))
   (if (null? u)
      (values (bytes->string (reverse key)) u)
      (if (or (eq? (car u) #\=)
              (eq? (car u) #\&))
         (values (bytes->string (reverse key)) u)
         ;else
         (loop (cdr u) (cons (car u) key))))))

(define (get-value u)
   (define (rev-loop a b)
      (if (null? a)
         b
         (if (and
               (eq? (car a) #\%)
               (not (null? b))
               (not (null? (cdr b))))
            ; % encoded character
            (let ((c (bor (<< (get hex-table (car b) 0) 4)
                              (get hex-table (cadr b) 0))))
               (if (null? (cdr a))
                  (cons c (cddr b))
                  (rev-loop (cddr a) (cons
                                        (cadr a)
                                        (cons c (cddr b))))))
            ; regular case
            (rev-loop (cdr a) (cons (car a) b)))))
   (let loop ((u u) (value #null))
      (if (null? u)
         (values (bytes->string (rev-loop value #null)) u)
         (if (eq? (car u) #\&)
            (values (bytes->string (rev-loop value #null)) (cdr u))
            ;else
            (loop (cdr u) (cons (car u) value))))))

(define (get-keyvalue u)
(let*((key u (get-key u)))
   (if (null? u)
      (values (cons key #null) u)
      (if (eq? (car u) #\&)
         (values (cons key #null) (cdr u))
         (let*((value u (get-value (cdr u))))
            (values (cons key value) u))))))


(define (http:parse-url url)
(let*((path u (get-path (string->runes url))))
   (let loop ((args #empty) (u u))
      (if (null? u) (tuple path args)
         (let*((kv u (get-keyvalue u)))
            (loop (put args (string->symbol (car kv)) (cdr kv)) u))))))


; https://en.wikipedia.org/wiki/Percent-encoding
; '[' and ']' are reserver characters, so should be encoded as '%5B' and '%5D'
(define (ends-with-vector arg)
   (if (less? (string-length arg) 6)
      #f
      (string-eq? (substring arg (- (string-length arg) 6) (string-length arg)) "%5B%5D")))

; new version with arrays support
(define (http:parse-url url)
(let*((path u (get-path (string->runes url))))
   (let loop ((args #empty) (u u))
      (if (null? u) (tuple path args)
         (let*((kv u (get-keyvalue u)))
            (let ((key (car kv))
                  (value (cdr kv)))
               ;(print "key: " key)
               ;(print "value: " value)
               (if (ends-with-vector key) ; encoded as blob?
                  ; slow and naive implementation:
                  (let ((key (string->symbol (substring key 0 (- (string-length key) 6)))))
                     ;(print "KEY: " key)
                     (loop (put args key
                              (list->vector
                                 (if (blob? (getf args key))
                                    (append (vector->list (getf args key)) (list value))
                                    (list value))))
                           u))
                  (loop (put args (string->symbol (car kv)) (cdr kv)) u))))))))


))
