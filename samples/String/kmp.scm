#!/usr/bin/ol

; please check https://srfi.schemers.org/srfi-13/srfi-13.html
(define (kmp p s start)
   (define (init-kmp p)
      (let*((m (string-length p))
            (next (make-vector m 0)))
         (let loop ((i 1) (j 0))
            (cond
             ((less? m i)
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
             (loop i (vector-ref next j)  si (skip (str-iter p) (vector-ref next j)) ))))))

(print (kmp "as" "abbsbshdgfhdgasbdgasfh" 0))
(print (kmp "as" "abbsbshdgfhdgasbdgasfh" 14))
