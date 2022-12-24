#!/usr/bin/env ol

(if (zero? (length *vm-args*))
   (runtime-error "Usage: repl-2-sexp.lis repl-binary"))

(import (only (lang eval) disassembly))

      (define (object->list obj)
         (let loop ((pos (size obj)) (tail #null))
            (if (eq? pos 0)
               tail
               (loop (-- pos) (cons (ref obj pos) tail)))))

      ; encode strings into independent intermediate format
      (define Q (string #\"))
      (define (->string o)
         (let ((o (if (symbol? o) (symbol->string o) o)))
            (string-append Q (s/\n/'3/g (s/"/'2/g (s/'/'1/g o))) Q))) ;" o)) Q)))

      (define (encode-number num tail)
         (cons (list "N" num) tail))

      ;; encode the value
      (define (encode-value val tail)
         (cons
            (case val
               (#null "NULL")
               (#empty "EMPTY")
               (#eof "EOF")
               (#true "TRUE")
               (#false "FALSE")

               (stdin "STDIN")
               (stdout "STDOUT")
               (stderr "STDERR")

               (else val))
            tail))

      ;; encode the integer number
      (define (encode-integer val tail)
         (cons (list "INTEGER" val) tail))

      (define (encode-fields lst pos index)
         (foldr (lambda (elem out)
               (cond
                  ((integer? elem)
                     (encode-integer elem out))
                  ((value? elem)
                     (encode-value elem out))
                  (else
                     (let ((target (index elem #false)))
                        (encode-number (- target pos) out)))))
         #null lst))

      ;; todo - pack type to this now that they fit 6 bits
      ;; encode the reference
      (define (encode-reference val pos index tail)
         ; сюда мы можем придти только если непосредственно энкодим число, как часть объекта оно сюда придти не может
         (define (->object val)
            (encode-fields (object->list val) pos index))

         
         (cons (case (type val)
                  (type-pair
                     (list "PAIR"   (->object val)))
                  ; strings and symbols
                  (type-string
                     (list "STRING" (->string val)))
                  (type-string-wide
                     (list "STRING" (->string val)))
                  (type-symbol
                     (list "SYMBOL" (->object val)))
                  (type-vector
                     (list "VECTOR" (->object val)))
                  ; functions
                  (type-bytecode
                     (list "BYTECODE" (map cdr ((disassembly val) 'disassembly))))
                  (type-closure
                     (list "CLOSURE" (->object val)))
                  (type-procedure
                     (list "PROCEDURE" (->object val)))
                  (type-bytevector
                     (list "BYTEVECTOR" (bytevector->list val)))
                  (type-constructor
                     (list "CONSTRUCTOR" (->object val)))
                  (type-inexact
                     (list "INEXACT" (bytevector->list val)))
                  (type-rational
                     (list "RATIONAL" (->object val)))
                  ; ff
                  (24 (list "BLACK" (->object val)))
                  (25 (list "BLACK-RIGHT" (->object val)))
                  (26 (list "RED" (->object val)))
                  (27 (list "RED-RIGHT" (->object val)))

                  ; other
                  (else
                     (runtime-error "unknown object" val)))
                     ;; (cons* "+"
                     ;; (if (ref val 0)
                     ;;    (list "R" (typename (type val))
                     ;;          (bytevector->list val))
                     ;;    (list "O" (typename (type val))
                     ;;          (->object val))))))
            tail))

      ; -----
      ; collect all objects into dictionary

      (define (collect loop seen obj)
         (let ((seen (put seen obj 1)))
            (if (ref obj 0) ; == binary object, 0 in ref works only for blobs
               seen
            else
               (fold loop seen (object->list obj)))))


      (define (object-closure root)
         (let loop ((seen #empty) (obj root))
            (cond
               ; целые числа
               ((integer? obj)
                  seen)
               ; остальные константы
               ((value? obj) ; todo: проверять размер value < 24-bits 
                  seen)
               ; если хотим статистику - (ff-update seen obj (+ n 1))
               ((seen obj #false) => (lambda (n)
                  seen))
               ; прочие объектыs
               (else
                  (collect loop seen obj)))))

      ; ---------------------
      ; enumerate all objects
      (define (index-closure clos)
         (ff-fold
            (λ (fc key _)
               (let* ((fp clos fc))
                  ; no more than 16777216 object for x32 is allowed
                  (cons (++ fp) (ff-update clos key fp))))
            (cons 0 clos) clos))


      ; root -> byte-stream processor
      (define (encode-object obj tail)
         ; generate subobjects index
         (let ((index (cdr (index-closure (object-closure obj)))))
            (let loop ((kvs (ff-iter index)))
               (cond
                  ((null? kvs) tail)
                  ((pair? kvs)
                     (let ((kv (car kvs)))
                        (encode-reference (car kv) (cdr kv) index
                           (lambda () (loop (cdr kvs))))))
                  (else
                     (loop (force kvs)))))))

; object -> serialized list
(define (text-encode obj)
   (force-ll (encode-object obj #n)))

; =========================================================
(print "(")
(for-each print
   (text-encode (fasl-load (car *vm-args*) #f)))
(print ")")
