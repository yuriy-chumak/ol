#!/usr/bin/ol

(define *vm-args* (list "#" "https://acomics.ru/~forgottenG" 0))

(define (kmp p s start)
   (define (init-kmp p)
      (let*((m (string-length p))
            (next (make-vector m 0)))
         (let loop ((i 1) (j 0))
            (cond
             ((>= i (- m 1))
                next)
             ((char=? (string-ref p i) (string-ref p j))
                (let ((i (+ i 1))
                      (j (+ j 1)))
                   (set-ref! next i j)
                   (loop i j)))
             ((= j 0)
                (let ((i (+ i 1)))
                   (set-ref! next i 0)
                   (loop i j)))
             (else
                (loop i (ref next j)))))))
   ;
   (let ((next (init-kmp p))
         (m (string-length p)) ; pattern length
         (n (string-length s))) ; string length
    (let loop ((i start) (j 0))
       (cond
          ((or (>= j m) (>= i n))
             (if (= j m)
                (- i m)))
          ((char=? (string-ref s i) (string-ref p j))
             (loop (+ i 1) (+ j 1)))
          ((= j 0)
             (loop (+ i 1) j))
          (else
             (loop i (blob-ref next j)))))))

(define (kmp p s start)
   (define (init-kmp p)
      (let*((m (string-length p))
            (next (make-vector m 0)))
         (let loop ((i 1) (j 0))
            (cond
             ((>= i (- m 1))
                next)
             ((char=? (string-ref p i) (string-ref p j))
                (let ((i (+ i 1))
                      (j (+ j 1)))
                   (set-ref! next i j)
                   (loop i j)))
             ((= j 0)
                (let ((i (+ i 1)))
                   (set-ref! next i 0)
                   (loop i j)))
             (else
                (loop i (ref next j)))))))
   (define (skip p n)
;      (print "skip: " p ", " n)
      (cond
         ((function? p)
            (skip (force p) n))
         ((or (eq? n 0)); (null? p))
            p)
         ((pair? p)
            (skip (cdr p) (- n 1)))))
;         (else
;            (skip (p) n))))
   ;
   (let ((next (init-kmp p))
         (m (string-length p)) ; pattern length
         (n (string-length s))) ; string length
   (let loop ((i start) (j 0) (si (skip (str-iter s) start)) (pj (str-iter p)))
;      (print "i: " i ", j:" j ", si: " si ", pj: " pj)
       (cond
         ((function? si)
            (loop i j (force si) pj))
          ((or (>= j m) (>= i n)) ; либо мы нашли строку, либо дошли до конца
             (if (= j m)
                (- i m)))
          ((char=? (car si) (car pj))
             (loop (+ i 1) (+ j 1) (force (cdr si)) (force (cdr pj))))
          ((= j 0)
             (loop (+ i 1) j (force (cdr si)) pj))
          (else
             (loop i (blob-ref next j)  si (skip (str-iter p) (blob-ref next j)) ))))))


(import (scheme misc)
        (otus random!))
(define (number->string n)
   (list->string (render-number n null 10)))
(define (system command) (syscall 1017 (c-string command) #f #f))
(define (system command) #t)

(define url (cadr *vm-args*))
(unless (string-eq? (substring url 0 20) "https://acomics.ru/~")
   (exit-owl (print "Урл мусить починатись з https://acomics.ru/~")))
(define temp (string-append "/tmp/ol-" (number->string (rand! #xffffffff))))

(define temp "/tmp/ol-19393") ; TEMP

(define (download url file)
   (let ((command-line (fold string-append "" (list
                           "wget -c -q -O " file " \"" url "\""))))
      (system command-line)))
(define (get-number string from)
   (let ((to (let loop ((i from))
                (unless (<= #\0 (string-ref string i) #\9) i (loop (+ i 1))))))
      (string->number (substring string from to) 10)))

; 1. Количество глав
(display "Пошук кiлькостi глав...")
(download url (string-append temp ".html"))

;(define chapters (string->number
;   ((string->regex "s/.*?(<span class=\"issueNumber\">\\d+\\/(\\d+)<\\/span>).*/\\2/g")
;      (list->string (file->list temp))) 10))
(define chapters-pattern "<span class=\"issueNumber\">")
(define chapters-html (list->string (file->list (string-append temp ".html"))))
(define chapters (get-number chapters-html (+ (kmp chapters-pattern chapters-html 0) 2 (string-length chapters-pattern))))

(print " " chapters)

(if (zero? chapters)
   (exit-owl 1))

(define folder (substring url 20 (string-length url)))
(display "Створюю папку ")(display folder)(display ".")
(syscall 1017 (c-string (string-append "mkdir " folder)) #f #f)
(syscall2 80 (c-string folder))
(print)

(display "Завантажую сторiнки: ")
(let loop ((chapter (if (> 2 (length *vm-args*)) (string->number (caddr *vm-args*)) 1)))
   (define page-url (string-append (string-append url "/") (number->string chapter)))
   ;(print "page-uri: " page-url)
   (download page-url temp)
   (define page-html (list->string (file->list temp)))
   (define page-pattern "<img id=\"mainImage\" src=\"")
;   (define image-url ((string->regex "s/.*?(<img id=\"mainImage\" src=\"(.*?)\").*/\\2/g")
;                     (list->string (file->list temp))))
   (define page-url-start (+ (kmp page-pattern page-html 0) (string-length page-pattern)))
   (define page-url-end (kmp "\"" page-html page-url-start))
   (define image-url (substring page-html page-url-start page-url-end))
   (display
      (unless (zero? (string-length image-url))
         (if (zero? (system (fold string-append "" (list "echo -c -q \"https://acomics.ru" image-url "\""))))
            "+" "-")
         "-"))

   (if (less? chapter chapters)
      (loop (+ chapter 1))))

(syscall 87 (c-string temp) #f #f)
(print "done.")
