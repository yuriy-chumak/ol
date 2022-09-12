(define-library (scheme list)
   (version 1.0)
   (license MIT/LGPL3)
   (keywords (scheme ol list))
   (description "
      List processing functions.")

   (import
      (scheme core))

   (export
      ;map for-each
      fold foldr)


(begin

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
