(define-library (lib sha1)
  (export
    sha1:digest
    base64:encode)
    
  (import (scheme core)
      (scheme list)
      (owl math) (owl list) (owl string) (owl list-extra))
(begin

(define (sha1-padding-size n)
   (let ((x (mod (- 56 (rem n 64)) 64)))
      (if (= x 0) 64 x)))

(define (sha1-pad-message message)
   (let*((message-len (string-length message))
         (message-len-in-bits (* message-len 8))
         (buffer-len (+ message-len 8 (sha1-padding-size message-len)))
         (message (string-append message (runes->string '(#b10000000))))
         (zeroes-len (- buffer-len message-len 1 4)) ; for ending length encoded value
         (message (string-append message (make-string zeroes-len 0)))
         (message (string-append message (runes->string (list
            (band (>> message-len-in-bits 24) #xFF)
            (band (>> message-len-in-bits 16) #xFF)
            (band (>> message-len-in-bits  8) #xFF)
            (band (>> message-len-in-bits  0) #xFF))))))
;      (print "message-len: " message-len)
;      (print "message-len-in-bits: " message-len-in-bits)
;      (print "buffer-len: " buffer-len)
;      (print "zeroes-len: " zeroes-len)
;      (print "message: " message)
;      (print "length(message): " (string-length message))
      message))

(define XOR (lambda args (fold bxor 0 args)))
(define OR (lambda args (fold bor 0 args)))
(define NOT (lambda (arg) (bxor arg #xFFFFFFFF)))

(define (->32 i)
   (band i #xFFFFFFFF))
(define (rol bits x)
   (->32
      (bor
         (<< x bits)
         (>> x (- 32 bits)))))

(define (word->list x)
   (list
      (band (>> x 24) #xFF)
      (band (>> x 16) #xFF)
      (band (>> x  8) #xFF)
      (band (>> x  0) #xFF)))
         
(define (message->words message)
;  (print (string-ref message 0) ":" message ", " (string-length message))
   (let cycle ((W
               (let loop ((t (lrange 0 1 16)))
                  (if (null? t)
                     null
                  (let*((p (* (car t) 4)))
                     (cons (OR
                              (<< (string-ref message (+ p 0)) 24)
                              (<< (string-ref message (+ p 1)) 16)
                              (<< (string-ref message (+ p 2))  8)
                              (<< (string-ref message (+ p 3))  0))
                           (loop (cdr t)))))))
               (t 16))
;     (print "\n\nW: " W)
      (if (eq? t 80)
         W
         (cycle (append W (list
            (XOR
               (rol 1 (list-ref W (- t 3)))
               (rol 1 (list-ref W (- t 8)))
               (rol 1 (list-ref W (- t 14)))
               (rol 1 (list-ref W (- t 16))))))
            (+ t 1)))))

(define (sha1:digest message)
   (let*((h0 #x67452301)
         (h1 #xEFCDAB89)
         (h2 #x98BADCFE)
         (h3 #x10325476)
         (h4 #xC3D2E1F0)
         (K '(#x5A827999 #x6ED9EBA1 #x8F1BBCDC #xCA62C1D6))
         (padded-message (sha1-pad-message message))
         (n (/ (string-length padded-message) 64)))
;      (print "padded-message: " padded-message)
;      (print "n: " n)

      (let main ((i 0)
                 (A h0) (B h1) (C h2) (D h3) (E h4))
         (if (= i n)
            (fold append null
               (list (word->list A) (word->list B) (word->list C) (word->list D) (word->list E)))
            (let*((message (substring padded-message (* i 64) (+ (* i 64) 64)))
                  (W (message->words message)))
               (let*((a b c d e ; round 1
                        (let loop ((a A) (b B) (c C) (d D) (e E) (t 0))
;                           (print "abcde " a " " b " " c " " d " " e)
                           (if (< t 20)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band (NOT b) d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 0)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 2
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 20))
;                           (print "abcde " a " " b " " c " " d " " e)
                           (if (< t 40)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 1)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 3
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 40))
;                           (print "abcde " a " " b " " c " " d " " e)
                           (if (< t 60)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (OR (band b c) (band b d) (band c d))
                                             e
                                             (list-ref W t)
                                             (list-ref K 2)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e))))
                     (a b c d e ; round 4
                        (let loop ((a a) (b b) (c c) (d d) (e e) (t 60))
;                           (print "abcde " a " " b " " c " " d " " e)
                           (if (< t 80)
                              (loop (->32
                                          (+ (rol 5 a)
                                             (XOR b c d)
                                             e
                                             (list-ref W t)
                                             (list-ref K 3)))
                                    a
                                    (rol 30 b)
                                    c
                                    d
                                    (+ t 1))
                              (values a b c d e)))))
                              
                  (main (+ i 1)
                     (->32 (+ A a))
                     (->32 (+ B b))
                     (->32 (+ C c))
                     (->32 (+ D d))
                     (->32 (+ E e)))))))))

(define (base64:encode L)
(let ((alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
      (len (length L)))
   (runes->string
   (let loop ((i 0))
      (let*((c0 (if (< i len) (list-ref L i) 0))
            (i (+ i 1))
            (c1 (if (< i len) (list-ref L i) 0))
            (i (+ i 1))
            (c2 (if (< i len) (list-ref L i) 0))
            (i (+ i 1))
            (q (- i len))
            (u (OR
                  (<< c0  16)
                  (<< c1   8)
                  (<< c2   0))))
         (cons 
         (string-ref alpha       (>> u 18)    )
         (cons
         (string-ref alpha (band (>> u 12) 63))
         (cons
         (if (> q 1) #\=
         (string-ref alpha (band (>> u  6) 63)))
         (cons
         (if (> q 0) #\=
         (string-ref alpha (band     u     63)))
         (if (< i len) (loop i) null))))))))))

;            0         1         2         3         4         5
;(print (base64
;;   (sha1-digest "12345678901234567890123456789012345678901234567890")))
;   (sha1-digest "DgH3uuAMD6YVe1XYEfyaqQ==258EAFA5-E914-47DA-95CA-C5AB0DC85B11")))
;(print (base64 '(49 49 49)))
   
))
