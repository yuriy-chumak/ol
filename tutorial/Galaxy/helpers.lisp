(define (string-split string delimiter)
(let ((n (string-length string)))
   (let loop ((i 0) (p 1) (tail '()))
      (if (eq? p n)
         (reverse (cons (substring string i p) tail))
         (if (eq? (string-ref string p) delimiter)
            (loop (+ p 1) (+ p 2) (cons (substring string i p) tail))
            (loop i (+ p 1) tail))))))

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
