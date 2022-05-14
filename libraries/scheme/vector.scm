(define-library (scheme vector) ; srfi-133
   (version 1.1)
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
   ; [obj ...], '[obj ...] and `[,obj ...]    * ol specific
   ; 

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

      vector-copy
      vector-copy!
      vector-append
      vector-fill!

      vector-for-each
      vector-map
      vector-fold
      vector-foldr
   )

   (import
      (scheme core)
      (scheme srfi-1)
      (owl list)
      (owl string)
      (owl math))

(begin
   ; The *length* of a vector is the number of elements that it
   ; contains. This number is a non-negative integer that is
   ; fixed when the vector is created. The *valid indexes* of a
   ; vector are the exact non-negative integers less than the
   ; length of the vector. The first element in a vector is indexed
   ; by zero, and the last element is indexed by one less than
   ; the length of the vector.

   ; internal helpers
   (define (make vec start end)
      ; makes a list from vector part
      (let loop ((pos end) (tail #null))
         (if (less? pos start)
            tail
            (loop (-- pos) (cons (ref vec pos) tail)))))

   (define (copy! to at from start end)
      ; note: ref and set-ref for vectors starts from 1 not 0
      (let loop ((start (++ start)) (p (++ at)))
         (when (or (less? start end) (eq? start end)) ; <=
            (set-ref! to p (ref from start))
            (loop (++ start) (++ p))))
      to)
   (define (copy from start end)
      (define out (make-vector (- end start)))
      (copy! out 0 from start end))

   (define (fill! to what start end)
      ; note: ref and set-ref for vectors starts from 1 not 0
      (let loop ((p (++ start)))
         (when (or (less? p end) (eq? p end))
            (set-ref! to p what)
            (loop (++ p))))
      to)


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
      (ref vec (++ k)))

   (assert (vector-ref #(1 2 3 4 5 8 13 21) 0)  ===>  1)
   (assert (vector-ref #(1 2 3 4 5 8 13 21) 5)  ===>  8)
   (assert (vector-ref #(1 2 3 4 5 8 13 21) 9)  ===>  #false)

   ; procedure:  (vector-set! vector k obj)
   ;
   ; The vector-set! procedure stores *obj* in element *k* of *vector*.
   
   (define (vector-set! vec k obj)
      (set-ref! vec (++ k) obj))

   (assert (vector-set! #(1 2 3) 1 7) ===>  #(1 7 3))
   (assert (vector-set! #(1 2 3) 8 7) ===>  #(1 2 3))

   ; procedure:  (vector->list vector)
   ; procedure:  (vector->list vector start)
   ; procedure:  (vector->list vector start end)

   (define vector->list
      (case-lambda
         ((vec)           (make vec            1 (size vec)))
         ((vec start)     (make vec (++ start) (size vec)))
         ((vec start end) (make vec (++ start) end))))

   (assert (vector->list #(1 2 3 4 5))     ===> '(1 2 3 4 5))
   (assert (vector->list #(1 2 3 4 5) 1)   ===> '(2 3 4 5))
   (assert (vector->list #(1 2 3 4 5) 2 5) ===> '(3 4 5))
   (assert (vector->list #(1 2 3 4 5) 2 7) ===> '(3 4 5 #f #f))

   ; procedure:  (list->vector list)

   (define (list->vector l)
      (vm:make type-vector l))

   (assert (list->vector '())              ===> #())
   (assert (list->vector '(1 2 3 4 #t))    ===> #(1 2 3 4 #t))

   ; procedure:  (vector->string vector)
   ; procedure:  (vector->string vector start)
   ; procedure:  (vector->string vector start end)

   (define vector->string (case-lambda 
      ((vec)           (runes->string (vector->list vec)))
      ((vec start)     (runes->string (vector->list vec start)))
      ((vec start end) (runes->string (vector->list vec start end)))))

   (assert (vector->string #(#\a #\b #\c #\d #\e))        ===>  "abcde")
   (assert (vector->string #(#\a #\b #\c #\d #\e) 2)      ===>  "cde")
   (assert (vector->string #(#\a #\b #\c #\d #\e) 2 5)    ===>  "cde")
   (assert (vector->string #(#\a #\b #\c #\d #\e) 2 8)    ===>  #false)

   ; procedure:  (string->vector string)
   ; procedure:  (string->vector string start)
   ; procedure:  (string->vector string start end)

   (define string->vector (case-lambda
      ((str)           (list->vector (string->runes str)))
      ((str start)     (list->vector (string->runes (substring str start (string-length str)))))
      ((str start end) (list->vector (string->runes (substring str start end))))))

   (assert (string->vector "abcde")       ===> #(#\a #\b #\c #\d #\e))
   (assert (string->vector "abcde" 2)     ===> #(#\c #\d #\e))
   (assert (string->vector "abcde" 2 5)   ===> #(#\c #\d #\e))


   ; procedure:  (vector-copy vector)
   ; procedure:  (vector-copy vector start)
   ; procedure:  (vector-copy vector start end)
   (define vector-copy (case-lambda
      ((vec)           (vm:cast vec (type vec)))
      ((vec start)     (list->vector (vector->list vec start)))
      ((vec start end) (list->vector (vector->list vec start end)))))

   (assert (vector-copy #(1 2 3 4 5))                       ===> #(1 2 3 4 5))
   (assert (eq? #(1 2 3 4 5) (vector-copy #(1 2 3 4 5)))    ===> #false)
   (assert (equal? #(1 2 3 4 5) (vector-copy #(1 2 3 4 5))) ===> #true)
   (assert (vector-copy #(1 2 3 4 5) 0)                     ===> #(1 2 3 4 5))
   (assert (vector-copy #(1 2 3 4 5) 3)                     ===> #(4 5))
   (assert (vector-copy #(1 2 3 4 5) 5)                     ===> #())
   (assert (vector-copy #(1 2 3 4 5) 3 3)                   ===> #())
   (assert (vector-copy #(1 2 3 4 5) 3 5)                   ===> #(4 5))
   (assert (vector-copy #(1 2 3 4 5) 5 3)                   ===> #())

   ; procedure:  (vector-copy! to at from)
   ; procedure:  (vector-copy! to at from start)
   ; procedure:  (vector-copy! to at from start end)
   (define vector-copy! (case-lambda
      ((to at from start end) (copy! to at from start end))
      ((to at from start)     (copy! to at from start (size from)))
      ((to at from)           (copy! to at from     0 (size from)))))

   ; procedure:  (vector-append vector ...)
   (define (vector-append vector . tail)
      (fold append (vector->list vector)
         (map vector->list tail)))

   ; procedure:  (vector-fill! vector fill)
   ; procedure:  (vector-fill! vector fill start)
   ; procedure:  (vector-fill! vector fill start end)
   (define vector-fill! (case-lambda
      ((vec fill)           (fill! vec fill     0 (size vec)))
      ((vec fill start)     (fill! vec fill start (size vec)))
      ((vec fill start end) (fill! vec fill start end))))

   ; additional
   ; 6.10  Control features
   (define vector-for-each (case-lambda
      ((f a)      (for-each f (vector->list a)))
      ((f a b)    (for-each f (vector->list a) (vector->list b)))
      ((f a b . c)(apply for-each (cons f (map vector->list (cons a (cons b c))))))
      ((f) #false)))

   (define vector-map (case-lambda
      ((f a)      (list->vector
                     (map f (vector->list a))))
      ((f a b)    (list->vector
                     (map f (vector->list a) (vector->list b))))
      ((f a b . c)(list->vector
                     (apply map (cons f (map vector->list (cons a (cons b c)))))))
      ((f) #false)))

      (assert (vector-map (lambda (x) x) #(1 2 3 4 5))  ===>  #(1 2 3 4 5))
      (assert (vector-map (lambda (x y) (cons x y))
                  #(1 2 3) #(9 8 7))                    ===>  #((1 . 9) (2 . 8) (3 . 7)))

   ; TODO: optimize
   (define vector-fold (case-lambda
      ((f z a)       (fold f z (vector->list a)))
      ((f z a b)     (fold f z (vector->list a) (vector->list b)))
      ((f z a b . c) (apply fold (cons* f z (map vector->list (cons a (cons b c))))))
      ((f z) z)))

      (assert (vector-fold + 7 [1 2 3 4 5])  ===>  22)
      (assert (vector-fold + -1
                         #(1 2 3) #(9 8 7))  ===>  29)

   ; TODO: optimize
   (define vector-foldr (case-lambda
      ((f z a)       (foldr f z (vector->list a)))
      ((f z a b)     (foldr f z (vector->list a) (vector->list b)))
      ((f z a b . c) (apply foldr (cons* f z (map vector->list (cons a (cons b c))))))
      ((f z) z)))

      (assert (vector-foldr - 7 [1 2 3 4 5]) ===>  -4) ; (- 1 (- 2 (- 3 (- 4 (- 5 7)))))
      (assert (vector-foldr - -1
                         #(1 2 3) #(9 8 7))  ===>  -5) ; (- 1 9 (- 2 8 (- 3 7 -1)))

))
