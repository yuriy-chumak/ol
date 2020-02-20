(define-library (scheme bytevector)
   (version 1.1)
   (license MIT/LGPL3)
   (keywords (otus ol scheme bytevector))
   (description "
      Otus-Lisp bytevectors support library.")

   ; Bytevectors represent blocks of binary data. They are
   ; fixed-length sequences of bytes, where a byte is an exact
   ; integer in the range from 0 to 255 inclusive. A bytevector
   ; is typically more space-efficient than a vector containing
   ; the same values.      
   ;
   ; Bytevectors are written using the notation #u8(byte...).
   ; For example, a bytevector of length 3 containing the byte
   ; 0 in element 0, the byte 10 in element 1, and the byte 5 in
   ; element 2 can be written as follows:
   ;
   ;      #u8(0 10 5)
   ;
   ; Bytevector constants are self-evaluating, so they do not
   ; need to be quoted in programs.

   (export
      bytevector?     ; * (scheme core)
      make-bytevector ; * (scheme core)
      bytevector      ; * (scheme core)

      bytevector-length
      bytevector-u8-ref
      bytevector-u8-set!

      bytevector-copy
      bytevector-copy!
      bytevector-append

      utf8->string
      string->utf8

      bytevector->list ; * ol specific
      list->bytevector ; * ol specific
   )

   (import
      (scheme core)
      (owl list)
      (owl string)
      (owl math))

(begin
   ; The length of a bytevector is the number of elements that
   ; it contains. This number is a non-negative integer that is
   ; fixed when the bytevector is created. The valid indexes of
   ; a bytevector are the exact non-negative integers less than
   ; the length of the bytevector, starting at index zero as with
   ; vectors.

   ; * ol specific:
   (define (bytevector->list bv)
      (let ((len (size bv)))
         (if (eq? len 0)
            #null
            (let loop ((pos (|-1| len)) (tail #null))
               (if (eq? pos 0)
                  (cons (ref bv 0) tail)
                  (loop (|-1| pos) (cons (ref bv pos) tail)))))))

   ; * ol specific:
   (define (list->bytevector l)
      (make-bytevector l))

   ; procedure:  (bytevector-length bytevector)
   ;
   ; Returns the length of bytevector in bytes as an exact integer.

   (define bytevector-length size)

   ; procedure:  (bytevector-u8-ref bytevector k)
   ;
   ; Returns kth byte of bytevector.

   (define bytevector-u8-ref ref)

   ; procedure:  (bytevector-u8-set! bytevector k byte)
   ;
   ; Stores byte as the kth byte of bytevector.
   (define (bytevector-u8-set! bv k byte)
      (set-ref! bv k byte))

   ; internal helpers
   (define (copy! to at from start end)  ; * internal helper
      (let loop ((start start) (p at))
         (when (less? start end)
            (set-ref! to p (ref from start))
            (loop (|+1| start) (|+1| p))))
      to)
   (define (copy from start end)  ; * internal helper
      (define out (make-bytevector (- end start)))
      (copy! out 0 from start end))


   ; procedure:  (bytevector-copy bytevector)
   ; procedure:  (bytevector-copy bytevector start)
   ; procedure:  (bytevector-copy bytevector start end)

   (define bytevector-copy (case-lambda
      ((from start end) (copy from start end))
      ((from start)     (copy from start (size from)))
      ((from)          ;(copy from     0 (size from)))))
         (vm:cast from (type from))))) ; optimized version

   (assert (bytevector-copy (bytevector 1 2 3 4 5))      ===> (bytevector 1 2 3 4 5))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 0)    ===> (bytevector 1 2 3 4 5))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 3)    ===> (bytevector 4 5))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 8)    ===> #false)
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 0 0)  ===> (bytevector))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 0 3)  ===> (bytevector 1 2 3))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 0 5)  ===> (bytevector 1 2 3 4 5))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 3 4)  ===> (bytevector 4))
   (assert (bytevector-copy (bytevector 1 2 3 4 5) 3 5)  ===> (bytevector 4 5))


   ; procedure:  (bytevector-copy! to at from)
   ; procedure:  (bytevector-copy! to at from start)
   ; procedure:  (bytevector-copy! to at from start end)

   (define bytevector-copy! (case-lambda
      ((to at from start end) (copy! to at from start end))
      ((to at from start)     (copy! to at from start (size from)))
      ((to at from)           (copy! to at from     0 (size from)))))

   ; procedure:  (bytevector-append bytevector ...)
   (define bytevector-append (case-lambda
      (() (bytevector))
      ((a)
         (bytevector-copy a))
      ((a . b)
         (define bytevectors (cons a b))
         (define sizes (map size bytevectors))
         (define out (make-bytevector (apply + sizes)))
         (let loop ((at 0) (bytevectors bytevectors) (sizes sizes))
            (unless (null? bytevectors)
               (copy! out at (car bytevectors) 0 (car sizes))
               (loop (+ at (car sizes)) (cdr bytevectors) (cdr sizes))))
         out)))

   (assert (bytevector-append)                           ===> (bytevector))
   (assert (bytevector-append
      (bytevector 1 2 3))                                ===> (bytevector 1 2 3))
   (assert (bytevector-append
      (bytevector 1 2 3)
      (bytevector 4 5 6))                                ===> (bytevector 1 2 3 4 5 6))
   (assert (bytevector-append
      (bytevector 1 2 3)
      (bytevector 7 8 2)
      (bytevector 4 0))                                  ===> (bytevector 1 2 3 7 8 2 4 0))

   ; procedure:  (utf8->string bytevector)
   ; procedure:  (utf8->string bytevector start)
   ; procedure:  (utf8->string bytevector start end)

   (define (utf8->string bvec)
      (bytes->string (bytevector->list bvec)))

   ; procedure:  (string->utf8 string)
   ; procedure:  (string->utf8 string start)
   ; procedure:  (string->utf8 string start end)

   (define (string->utf8 str)
      (make-bytevector (string->bytes str)))

))
