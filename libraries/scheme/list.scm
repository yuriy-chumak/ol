(define-library (scheme list)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme ol list))
   (description
      "Extra list functions.")

   (import
      (scheme core))

   (export
      map for-each
      fold foldr
      
      has? ; * ol specific, fast version of member
      each
   )


(begin

   (define (has? lst x)
      (unless (null? lst)
         (or (eq? (car lst) x)
            (has? (cdr lst) x))))

   ;; (define (has? lst x)
   ;;    (unless (null? lst)
   ;;       (if (eq? (car lst) x)
   ;;          #true
   ;;          (has? (cdr lst) x))))


   ; procedure:  (map proc list1 list2 ...)
   ;
   (define map
      (define map (lambda (f a)
         (let loop ((a a))
            (if (null? a)
               #null
               (cons (f (car a)) (loop (cdr a)))))))
      (case-lambda
         ((f a)      (map f a))
         ((f a b)    (let loop ((a a)(b b)) ; map2
                        (if (null? a)
                           #null
                           (cons (f (car a) (car b)) (loop (cdr a) (cdr b))))))
         ; possible speedup:
         ;((f a b c) (let loop ((a a)(b b)(c c))
         ;              (if (null? a)
         ;                 #null
         ;                 (cons (f (car a) (car b) (car c)) (loop (cdr a) (cdr b) (cdr c))))))
         ((f a b . c) ; mapN
                     (let loop ((args (cons a (cons b c))))
                        (if (null? (car args)) ; закончились
                           #null
                           (cons (apply f (map car args)) (loop (map cdr args))))))
         ((f ) #null)))

   (assert (map cadr '((a b) (d e) (g h)))  ===> '(b e h))


   ; procedure:  (for-each proc list1 list2 ...)  * (scheme base)
   (define for-each (case-lambda
      ((f a)      (let loop ((a a))
                     (unless (null? a)
                        (f (car a))
                        (loop (cdr a)))))
      ((f a b)    (let loop ((a a) (b b))
                     (unless (null? a)
                        (f (car a) (car b))
                        (loop (cdr a) (cdr b)))))
      ((f a b . c)
                  (let loop ((a (cons a (cons b c))))
                     (unless (null? (car a)) ; закончились
                        (apply f (map car a))
                        (loop (map cdr a)))))
      ((f) #false)))

   ; procedure:  (for-each proc list1 list2 ...)  * (scheme base)
   (define each (case-lambda
      ((ok? a)    (call/cc (lambda (ret)
                     (let loop ((a a))
                        (unless (null? a)
                           (unless (ok? (car a))
                              (ret #false))
                           (loop (cdr a))))
                     #true)))
      ((ok? a b)  (call/cc (lambda (ret)
                     (let loop ((a a) (b b))
                        (unless (null? a)
                           (unless (ok? (car a) (car b))
                              (ret #false))
                           (loop (cdr a) (cdr b))))
                     #true)))
      ((ok? a . bs)
                  (call/cc (lambda (ret)
                     (let loop ((a (cons a bs)))
                        (unless (null? (car a))
                           (unless (apply ok? (map car a))
                              (ret #false))
                           (loop (map cdr a))))
                     #true)))
      ))

   ; procedure:  (fold proc list1 list2 ...)  * ol specific, fold left
   (define fold
      ; (fold (lambda (state a)) state a)
      ; (fold (lambda (state a b)) state a b)
      ; (fold (lambda (state a...)) state a...)
      (case-lambda
         ((f state a)      (let loop ((state state) (a a))
                              (if (null? a)
                                 state
                                 (loop (f state (car a)) (cdr a)))))
         ((f state a b)    (let loop ((state state) (a a) (b b))
                              (if (null? a)
                                 state
                                 (loop (f state (car a) (car b)) (cdr a) (cdr b)))))
         ((f state a b . c)
                           (let loop ((state state) (args (cons* a b c)))
                              (if (null? (car args))
                                 state
                                 (loop (apply f (cons state (map car args))) (map cdr args)))))
         ((f state) state)))

   ;(assert (fold - 9)                              ===>   9)
   ;(assert (fold - 9 '(1 2))                       ===>   6)
   ;(assert (fold - 9 '(1 2) '(3 4))                ===>  -1)
   ;(assert (fold - 9 '(1 2) '(3 4) '(5 6))         ===> -12)
   ;(assert (fold - 9 '(1 2) '(3 4) '(5 6) '(7 8))  ===> -27)

   ; procedure:  (foldr proc list1 list2 ...)  * ol specific, fold right
   (define foldr
      ; (foldr (lambda (a state)) state a)
      ; (foldr (lambda (a b state)) state a b)
      ; (foldr (lambda (a... state)) state a...)
      (case-lambda
         ((f state a)      (let loop ((state state) (a a))
                              (if (null? a)
                                 state
                                 (f (car a) (loop state (cdr a))))))
         ((f state a b)    (let loop ((state state) (a a) (b b))
                              (if (null? a)
                                 state
                                 (f (car a) (car b) (loop state (cdr a) (cdr b))))))
         ((f state a b . c)
                           (let loop ((state state) (args (cons* a b c)))
                              (if (null? (car args))
                                 state
                                 (apply f (append (map car args) (list (loop state (map cdr args))))))))
         ((f state) state)))

      ;(assert (foldr - 9)                             ===>  9)
      ;(assert (foldr - 9 '(1 2))                      ===>  8)
      ;(assert (foldr - 9 '(1 2) '(3 4))               ===>  9)
      ;(assert (foldr - 9 '(1 2) '(3 4) '(5 6))        ===> 10)
      ;(assert (foldr - 9 '(1 2) '(3 4) '(5 6) '(7 8)) ===> 11)
))
