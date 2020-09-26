(define-library (otus base64)
(export
   encode
   decode
)

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
         (let ((code (let loop ((hold [0 0 (str-iter str)]) (n 0))
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
   (define (bits n hold)
      (let loop ((hold hold))
         (vector-apply hold (lambda (v i l)
            (cond
               ((null? l)
                  (values (>> v (- i n)) #false))
               ((pair? l)
                  (if (not (less? i n))
                     (values (>> v (- i n)) (vector (band v (- (<< 1 (- i n)) 1)) (- i n) l))
                     (loop (vector
                        (bor (<< v 6) (kernel (car l) 0))
                        (+ i 6)
                        (unless (eq? (car l) "=") (cdr l))))))
               (else
                  (loop (vector v i (l)))))))))

   (define (decode str)
      (runes->string
         (let loop ((hold [0 0 (str-iter str)]))
            (let*((bit hold (bits 8 hold)))
               (if (not (zero? bit))
                  (cons
                     bit
                     (loop hold))
                  (if hold
                     (loop hold)
                     #null))))))

))