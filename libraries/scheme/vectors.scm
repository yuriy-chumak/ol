(define-library (scheme vectors) ; srfi-133
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (otus ol vector scheme))
   (description "
      Otus-Lisp vectors support library.")

   ; Vectors are heterogeneous structures whose elements are
   ; indexed by integers. A vector typically occupies less space
   ; than a list of the same length, and the average time needed
   ; to access a randomly chosen element is typically less for the
   ; vector than for the list.
   ;
   ; Vectors are written using the notation #(obj ...). For
   ; example, a vector of length 3 containing the number zero
   ; in element 0, the list (2 2 2 2) in element 1, and the
   ; string "Anna" in element 2 can be written as follows:
   ;
   ;      #(0 (2 2 2 2) "Anna")
   ;
   ; Additionally vectors can be written using the notation
   ; [obj ...]     * ol specific

   (export
      vector?     ; * (scheme core)
      make-vector ; * (scheme core)
      vector      ; * (scheme core)

      vector-length
      vector-ref
      vector-set!

      vector->list
      list->vector
      vector->string
      string->vector

      ;vector-copy
      ;vector-copy!
      vector-append
      ;vector-fill!

      vector-for-each
   )

   (import
      (scheme core)
      (owl list)
      (owl string))

(begin
   ; The *length* of a vector is the number of elements that it
   ; contains. This number is a non-negative integer that is
   ; fixed when the vector is created. The *valid indexes* of a
   ; vector are the exact non-negative integers less than the
   ; length of the vector. The first element in a vector is indexed
   ; by zero, and the last element is indexed by one less than
   ; the length of the vector.

   ; procedure:  (vector-length vector)
   ;
   ; Returns the number of elements in *vector* as an exact integer.

   (define (vector-length o)
      (size o))

   ; procedure:  (vector-ref vector k)
   ;
   ; The vector-ref procedure returns the contents of element
   ; k of vector

   (define (vector-ref vec k)
      (ref vec (|+1| k)))

   (assert (vector-ref #(1 2 3 4 5 8 13 21) 5) ===>  8)

   ; procedure:  (vector-set! vector k obj)
   ;
   ; The vector-set! procedure stores *obj* in element *k* of *vector*.
   
   (define (vector-set! vec k obj)
      (set-ref! vec (|+1| k) obj))

   ; procedure:  (vector->list vector)
   ; procedure:  (vector->list vector start)
   ; procedure:  (vector->list vector start end)

   (define vector->list
      (define (make vec start end)
         (let loop ((pos (|-1| end)) (tail #null))
            (if (less? pos start)
               tail
               (loop (|-1| pos) (cons (ref vec pos) tail)))))

      (case-lambda
         ((vec)
            (make vec 1 (|+1| (size vec))))
         ((vec start)
            (make vec start (|+1| (size vec))))
         ((vec start end)
            (make vec start end))))

   ; procedure:  (list->vector list)

   (define list->vector make-vector)

   ; procedure:  (vector->string vector)
   ; procedure:  (vector->string vector start)
   ; procedure:  (vector->string vector start end)

   (define vector->string runes->string)

   ; procedure:  (string->vector string)
   ; procedure:  (string->vector string start)
   ; procedure:  (string->vector string start end)

   (define string->vector string->runes)

   ; procedure:  (vector-copy vector)
   ; procedure:  (vector-copy vector start)
   ; procedure:  (vector-copy vector start end)

   ; procedure:  (vector-copy! to at from)
   ; procedure:  (vector-copy! to at from start)
   ; procedure:  (vector-copy! to at from start end)

   ; procedure:  (vector-append vector ...)
   (define (vector-append vector . tail)
      (fold append (vector->list vector)
         (map vector->list tail)))

   ; procedure:  (vector-fill! vector fill)
   ; procedure:  (vector-fill! vector fill start)
   ; procedure:  (vector-fill! vector fill start end)

   ; additional
   ; 6.10  Control features
   (define vector-for-each (case-lambda
      ((f a)      (for-each f (vector->list a)))
      ((f a b)    (for-each f (vector->list a) (vector->list b)))
      ((f a b . c)(apply for-each f (map vector->list (cons a (cons b c)))))
      ((f) #false)))

))
