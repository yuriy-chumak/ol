#!/usr/bin/env ol

(import (only (lang eval) disassembly))

; ----------------------------------------------------------------
; -- cache --
(define env (interaction-environment))
(define (->name env obj)
   (call/cc (lambda (return)
      (let loop ((kvs (ff-iter env)))
         (cond
            ((null? kvs) (return #false))
            ((pair? kvs)
               (let ((k (caar kvs))
                     (v (cdar kvs)))
                  (let ((v (ref v 2)))
                     (if (eq? v obj)
                        (return (symbol->string k)))
                     (let ((v (ref v 2)))
                        (if (eq? v obj)
                           (return (symbol->string k))))))
               (loop (cdr kvs)))
            (else (loop (kvs))))))))

;; (actor 'cache (lambda ()
;;    (let loop ((cache {}) (id 1))
;;       (let*((envelope (wait-mail))
;;             (sender msg envelope))
;;          (define got (cache msg #f))
;;          (if got
;;          then
;;             (mail sender (cons #true id))
;;             (loop cache id)
;;          else
;;             (define name (function->name env msg))
;;             (mail sender (cons #false (or name id)))
;;             (if name
;;                (loop (put cache msg name) id)
;;                (loop (put cache msg id) (+ id 1))))))))


(actor 'cache (lambda ()
   (let loop ((cache {}) (id 1))
      (let*((envelope (wait-mail))
            (sender msg envelope))
         (define got (cache msg #f))
         (if got
         then
            (mail sender (cons got #false))
            (loop cache id)
         else
            (define name (->name env msg))
            (define n_id (or name (number->string id)))
            (mail sender (cons #false n_id))

            (if name
               (loop (put cache msg name) id)
               (loop (put cache msg n_id) (+ id 1))))))))


(define Q (string #\"))
(define (->string o)
   (let ((o (if (symbol? o) (symbol->string o) o)))
      (string-append Q (s/"/'2/g (s/'/'1/g o)) Q))) ;" o) Q)))

(define-syntax C
   (syntax-rules (seen number->string)
      ((C name o)
         (if (car seen)
            (string-append "<" name "/" (car seen) ">")
            (cons
               (string-append name "/" (cdr seen))
               o)))))

(define (dump o)
   (if (value? o)
      (case o
         (#empty (cons* "EMPTY"))
         (#eof (cons* "EOF"))
         (else
            (case (type o)
               (type-port
                  (cons* "PORT" (vm:cast o type-enum+)))
               (else
                  o))))
   else
      (define seen (await (mail 'cache o)))

      ;; (print (typename (type o) #f) ": " o)
      (case (type o)
         ; functions
         (type-constructor
            (C "CONSTRUCTOR"
               (dump (ref o 1))))
         (type-bytecode
            ;; (print (vm:cast o type-bytevector))
            (C "BYTECODE"
               (map cdr ((disassembly o) 'disassembly))))
         (type-closure
            (C "CLOSURE"
               (vector->list (vector-map dump o))))
         (type-procedure
            (C "PROCEDURE"
               (vector->list (vector-map dump o))))

         ; numbers
         (type-int+ (C "INT+" o))
         (type-int- (C "INT-" o))
         (type-rational (C "RATIONAL"
            (cons (dump (car o)) (dump (cdr o)))))
         (type-complex (C "COMPLEX"
            (cons (dump (car o)) (dump (cdr o)))))
         (type-inexact (C "INEXACT"
            (bytevector->list (vm:cast o type-bytevector))))

         (type-port (C "PORT"
            (bytevector->list (vm:cast o type-bytevector))))

         ; strings
         (type-string
            (C "STRING"
               (->string o)))
         (type-string-wide
            (C "UNICODE"
               (->string o)))
         (type-symbol
            (C "SYMBOL"
               (->string o)))

         (type-vector
            (C "VECTOR"
               (vector->list (vector-map dump o))))
         (type-bytevector
            (C "BYTEVECTOR" (bytevector->list o)))


         ; ff?
         ((24 25 26 27)
            (C "FF" (map dump (ff->alist o))))

         ; cons/list
         (type-pair
            (C (if (list? o) "LIST" "PAIR")
               (if (list? o)
                  (map dump o)
                  (cons (dump (car o)) (dump (cdr o))))))

         ; others
         (else
            (print "ELSE " o)
            (C (symbol->string (typename (type o) "unknown"))
               #null))) ))

;; (print
;;    (dump (deserialize-file (or (list-ref *vm-args* 0) "test") #f) ))

(print
   (dump (interaction-environment) ))
