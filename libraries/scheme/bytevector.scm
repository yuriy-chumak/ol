(define-library (scheme bytevector)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol vector))
   (description "
      Otus-Lisp bytevectors support library.")

   ; (r7rs) 6.9  Bytevectors
   ;
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

      ; ...
      utf8->string
      string->utf8

      bytevector->list
      list->bytevector
   )

   (import
      (scheme core)
      (owl list)
      (owl string))

(begin
   ; The length of a bytevector is the number of elements that
   ; it contains. This number is a non-negative integer that is
   ; fixed when the bytevector is created. The valid indexes of
   ; a bytevector are the exact non-negative integers less than
   ; the length of the bytevector, starting at index zero as with
   ; vectors.

   ; procedure:  (bytevector-length bytevector)
   ;
   ; Returns the length of bytevector in bytes as an exact integer.

   (define bytevector-length size)

   ; procedure:  (bytevector-u8-ref bytevector k)
   ;
   ; Returns kth byte of bytevector.

   (define bytevector-u8-ref ref)

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


   ; procedure:  (bytevector-u8-set! bytevector k byte)
   ;
   ; Stores byte as the kth byte of bytevector.

   ; procedure:  (bytevector-copy bytevector)
   ; procedure:  (bytevector-copy bytevector start)
   ; procedure:  (bytevector-copy bytevector start end)

   ; procedure:  (bytevector-copy! to at from)
   ; procedure:  (bytevector-copy! to at from start)
   ; procedure:  (bytevector-copy! to at from start end)

   ; procedure:  (bytevector-append bytevector ...)

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
