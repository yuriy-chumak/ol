(define-library (otus base64)
(export
   encode
   decode)

(import (otus lisp))

(begin
   (setq alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

   ; -- encoder
   (define kernel (alist->ff (map cons (iota (string-length alphabet)) (string->bytes alphabet))))
   (define (bits n hold)
      (let loop ((hold hold))
         (vector-apply hold (lambda (v i l)
            (cond
               ; usual case
               ((pair? l)
                  (if (not (less? i n))
                     (values (>> v (- i n)) (vector (band v (- (<< 1 (- i n)) 1)) (- i n) l))
                     (loop (vector
                        (bor (<< v 8) (car l))
                        (+ i 8)
                        (cdr l)))))
               ; special case - no more characters in input stream
               ((null? l)
                  (cond
                     ((= i 0)
                        (values #false #false)) ; done
                     ((< i n)
                        (values (<< v (- n i)) (vector 0 0 #null)))
                     (else
                        (values (>> v (- i n)) (vector (band v (- (<< 1 (- i n)) 1)) (- i n) #null)))))
               ; just stream processing
               (else
                  (loop (vector v i (force l)))))))))

   (define (encode str)
      (runes->string
         (let*((data (cond
                        ((string? str) (str-iter str))
                        ((bytevector? str) (bytevector->list str))
                        (else
                           (fasl-encode str))))
               (code (let loop ((hold [0 0 data]) (n 0))
                        (let*((bit hold (bits 6 hold)))
                           (if bit
                              (cons
                                 (kernel bit)
                                 (loop hold (+ n 1)))
                              #null)))))
            (case (mod (length code) 4)
               (2 (append code '(#\= #\=)))
               (3 (append code '(#\=)))
               (else code)))))

   ; -- decoder
   (define kernel (alist->ff (map cons (string->bytes alphabet) (iota (string-length alphabet)))))

   (define (decode str)
      (define-values (data len)
      (let loop ((in (str-iter str)) (out 0) (len 0))
         (cond
            ((null? in)
               (values out len))
            ((pair? in)
               (define char (kernel (car in) #f)) ; 0..63, 6 bits
               (if char
                  (loop (cdr in) (bor (<< out 6) char) (+ len 1))
               else ; invalid char? (possibly '=', end-of-data)
                  (if (eq? (car in) #\=)
                     (loop (cdr in) (>> out 2) len)
                  else
                     (values out len))))
            (else
               (loop (force in) out len)))))
      (let loop ((i (div (* len 6) 8)) (data data) (out '()))
         (if (eq? i 0)
            out
         else
            (loop (- i 1) (>> data 8) (cons (band data 255) out)))) )

))