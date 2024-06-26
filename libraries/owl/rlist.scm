;;;
;;; random access lists
;;;

;; todo: the owl version seems nicer and iirc doesn't have the drawback of not being able contain internal nodes as values. switch to it later.

;; todo: use types from (scheme core)
;; todo: rins, rdel

; todo: rename to tree??
(define-library (owl rlist)

   (export
      rcons  ; O(1), (rcons a rl) -> rl'
      rcar   ; O(1), (rcar (rcons a rl)) -> a
      rcdr   ; O(1), (rcdr (rcons a rl)) -> rl'*
      rnull? ; O(1), plain null?
      rlist? ; O(1), obj -> bool
      rget   ; O(log n), (rget rl pos def) -> val | def if outside of rl
      rset   ; O(log n), (rset rl pos val) -> rl' | error if outside of rl
      rlen   ; O(log n)
      rfold  ; O(n), like fold
      rfoldr ; O(n), like foldr
      rmap   ; O(n), like map
      riter  ; O(n), rlist -> lazy list (aka iterator) **
      riterr ; O(n), ditto backwards
      requal?     ; O(n)***
      rlist->list ; O(n)
      list->rlist ; O(n log n), temp
      rlist       ; (rlist ...) -> rl
      rrev        ; O(n log n)
      )
      
      ; *   (equal? rl rl') = #true, but not necessarily (eq? rl rl')
      ; **  you can therefore use all lib-lazy list functions
      ; *** normal equal? will do because the representation is unique

   ; note, taken primitive type 10 for spines, variant 42 for nodes
   ; ie cannot store a node of type 42 into a rlist (which would be a
   ; major abstraction violation anyway).

   (import
      (scheme core)
      (scheme list)
      (owl math))

   (begin
      (define rnull? null?)
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define (rlist? l)
         (cond
            ((null? l) #true)
            ((eq? (type l) type-rlist-spine) #true)
            (else #false)))

      ;; consructors, later autogenerated

      (define-syntax node
         (syntax-rules ()
            ((node a l r) (vm:new type-rlist-node a l r))))

      (define-syntax if-node
         (syntax-rules (alloc)
            ((if-node val a b)
               (if (eq? (type val) type-rlist-node)
                  a b))))

      (define-syntax spine
         (syntax-rules ()
            ((spine w t rl) (vm:new type-rlist-spine w t rl))))

      ;; cons

      (define (rcons a rl)
         (if (null? rl)
            (spine 1 a rl)
            (lets ((w1 t1 tl rl))
               (if (null? tl)
                  (spine 1 a rl)
                  (lets ((w2 t2 ttl tl))
                     (if (= w1 w2)
                        (spine (+ (+ w1 w2) 1) (node a t1 t2) ttl)
                        (spine 1 a rl)))))))

      ;; car & cdr

      (define (rcar rl)
         (lets ((w a tl rl))
            (if-node a (ref a 1) a)))

      (define (rcdr rl)
         (lets ((w n tl rl))
            (if-node n
               (lets ((x a b n) (w (>> w 1)))
                  (spine w a (spine w b tl)))
               tl)))

      ;; get

      (define (rget-tree n w p)
         (if (eq? p 0)
            (if-node n (ref n 1) n)
            (lets ((a t1 t2 n) (wp (>> w 1)))
               (if (<= p wp)
                  (rget-tree t1 wp (- p 1))
                  (rget-tree t2 wp (- p (+ wp 1)))))))

      (define (rget rl pos def)
         (if (null? rl)
            def
            (lets ((w t tl rl))
               (if (< pos w)
                  (rget-tree t w pos)
                  (rget tl (- pos w) def)))))

      ;; set

      (define (rset-tree n w p v)
         (if (eq? p 0)
            (if-node n (set-ref n 1 v) v)
            (lets ((a t1 t2 n) (wp (>> w 1)))
               (if (<= p wp)
                  (set-ref n 2 (rset-tree t1 wp (- p 1) v))
                  (set-ref n 3 (rset-tree t2 wp (- p (+ wp 1)) v))))))

      (define (rset rl pos val)
         (if (null? rl)
            (runtime-error "rset: out of list setting " val)
            (lets ((w t tl rl))
               (if (< pos w)
                  (spine w (rset-tree t w pos val) tl)
                  (spine w t (rset tl (- pos w) val))))))

      ;; map

      (define (rmap-tree n op)
         (if-node n
            (lets ((a t1 t2 n))
               (node (op a)
                  (rmap-tree t1 op)
                  (rmap-tree t2 op)))
            (op n)))

      (define (rmap op rl)
         (if (null? rl)
            #null
            (lets ((w t tl rl))
               (spine w (rmap-tree t op) (rmap op tl)))))

      ;; riter (forwards)

      ; note, this generates (log t) of tree size t in one run
      ; to reduce the overhead. insert a lambda after the cons
      ; to get just one at a time.

      (define (riter-tree n tail)
         (if-node n
            (lets ((a t1 t2 n))
               (cons a
                  (riter-tree t1
                     (lambda () (riter-tree t2 tail)))))
            (cons n tail)))

      (define (riterator rl tail)
         (if (null? rl)
            tail
            (lets ((w t tl rl))
               (riter-tree t
                  (lambda () (riterator tl tail))))))

      (define (riter rl) (riterator rl #null))

      ;; riterr (backwards)

      (define (riterr-tree n tail)
         (if-node n
            (lets ((a t1 t2 n) (tail (cons a tail)))
               (riterr-tree t2
                  (lambda ()
                     (riterr-tree t1 tail))))
            (cons n tail)))

      (define (riteratorr rl tail)
         (if (null? rl)
            tail
            (lets ((w t tl rl))
               (riteratorr tl
                  (lambda ()
                     (riterr-tree t tail))))))

      (define (riterr rl) (riteratorr rl #null))

      ;; rfold (== (lfold op st (riter rl)))

      (define (rfold-tree op st n)
         (if-node n
            (lets
               ((a t1 t2 n)
                (st (op st a))
                (st (rfold-tree op st t1)))
               (rfold-tree op st t2))
            (op st n)))

      (define (rfold op st rl)
         (if (null? rl)
            st
            (lets ((w t tl rl))
               (rfold op (rfold-tree op st t) tl))))

      ;; rfoldr (== (lfoldr op st (riterr rl)))

      (define (rfoldr-tree op st n)
         (if-node n
            (lets
               ((a t1 t2 n)
                (st (rfoldr-tree op st t2))
                (st (rfoldr-tree op st t1)))
               (op a st))
            (op n st)))

      (define (rfoldr op st rl)
         (if (null? rl)
            st
            (lets ((w t tl rl))
               (rfoldr-tree op (rfoldr op st tl) t))))

      ;; conversions

      (define (list->rlist l) ; naive O(n log n)
         (foldr rcons #null l))

      (define (rlist->list rl)
         (rfoldr cons #null rl))

      ;; len

      (define (rlen rl)
         (let loop ((rl rl) (l 0))
            (if (null? rl)
               l
               (lets ((w a tl rl))
                  (loop tl (+ l w))))))

      ;; comparison

      (define requal? equal?)

      (define-syntax rlist
         (syntax-rules ()
            ((rlist) #null)
            ((rlist a . as)
               (rcons a (rlist . as)))))

   ;; note, could also be done in O(n)
   (define (rrev rl)
      (rfold (λ (out x) (rcons x out)) #null rl))

))



#|
;;
;; Another attempt.
;;

; make the obvious list of growing complete binary trees, but avoid the need
; to store numbers to denote their sizes by storing one bit of size information
; to the type and carefully maintaining a few invariants for the sizes:
;  - the first tree (if any) is always of depth 1
;  - there are at most 2 consecutive trees of the same depth
;  - the depths depth of the next tree is the same as current, or one deeper
; thus, depths (1 1), (1 1 2 3), (1 2 2 3) are ok, while (1 1 1 2), (2 2 4 8)
; and (1 4) are not. (5) is right out.

(define-module lib-rlist-owl

   (export
      rcons
      rlist?
      rcar
      rget)

   ;; use type 14
   (define (rlist? l)
      (cond
         ((null? l) #true)
         ((eq? (type l) type-rlist-spine) #true)
         ((eq? (type l) 46) #true) ;; <- ?
         (else #false)))

   ;; constructors

   (define-syntax same
      (syntax-rules ()
         ((same a r) (vm:new 14 a r))))

   (define-syntax less
      (syntax-rules ()
         ((less a r) (vm:new 46 a r))))

   (define-syntax node ;; in-tree node
      (syntax-rules ()
         ((node a b) (vm:new 78 a b))))

   ;; these are still not in use. type predicates and constructors later autogenerated.
   (define-syntax same?
      (syntax-rules () ((same? r) (eq? (type r) 14))))

   (define-syntax less?
      (syntax-rules () ((less? r) (eq? (type r) 46))))

   ; R = () | (less a1 ()) | (less a1 R) | (same a1 R)

   ; a is a value of *same size as head of r* (if any)
   ; O(log n)
   (define (rcons a r)
      (cond
         ((null? r)
            (less a r))
         ((eq? (type r) 14)
            (lets ((b rr r))
               (if (null? rr)
                  (less a r)
                  (lets ((c rr rr))
                     (less a (rcons (node b c) rr))))))
         (else
            (same a r))))

   ; O(1)
   (define (rcar r)
      (ref r 1))

   (define (ref-small-tree r p n)
      (if (eq? n 0)
         r
         (lets ((n _ (vm:shr n 1)))
            (if (eq? (vm:and p n) 0)
               (ref-small-tree (ref r 1) p n)
               (ref-small-tree (ref r 2) p n)))))

   (define (ref-tree r p n)
      (cond
         ((eq? n 0) r)
         ((eq? (type n) type-enum+)
            (if (eq? (type p) type-enum+)
               (ref-small-tree r p n)
               (ref-small-tree r (ncar p) n)))
         ((eq? (band p n) 0)
            (ref-tree (ref r 1) p (>> n 1)))
         (else
            (ref-tree (ref r 2) p (>> n 1)))))


   ;; rlist pos def → rlist[pos] | def
   (define (rget r p d)
      ;; find the correct tree
      (let loop ((r r) (p p) (n 1))
         (cond
            ((null? r) d)
            ((< p n)
               (if (eq? n 1)
                  (ref r 1)
                  (let ((n (>> n 1)))
                     (if (eq? (type n) type-enum+)
                        (ref-small-tree (ref r 1) p n)
                        (ref-tree (ref r 1) p n)))))
            ((eq? (type r) 14)
               (loop (ref r 2) (- p n) n))
            (else
               (loop (ref r 2) (- p n) (<< n 1))))))

   (define (rlen r)
      (let loop ((r r) (s 0) (n 1))
         (cond
            ((null? r) s)
            ((eq? (type r) 14)
               (loop (ref r 2) (+ s n) n))
            (else
               (loop (ref r 2) (+ s n) (<< n 1))))))

   ;;
   ;; Folds
   ;;

   (define (rfold-tree op st n)
      (if (eq? (type n) 78) ;; in-tree node. note, cannot store these in rlists (as in other data structures)
         (rfold-tree op
            (rfold-tree op st (ref n 1))
            (ref n 2))
         (op st n)))

   ;; O(n)
   (define (rfold op st rl)
      (if (null? rl)
         st
         (rfold op
            (rfold-tree op st (ref rl 1))
            (ref rl 2))))

   (define (rfoldr-tree op st n)
      (if (eq? (type n) 78) ;; in-tree node. note, cannot store these in rlists (as in other data structures)
         (rfoldr-tree op
            (rfoldr-tree op st (ref n 2))
            (ref n 1))
         (op n st)))

   ;; O(n)
   (define (rfoldr op st rl)
      (if (null? rl)
         st
         (rfoldr-tree op
            (rfoldr op st (ref rl 2))
            (ref rl 1))))

   ;;
   ;; Utils
   ;;

   ;; O(n log n)
   (define (rapp ra rb)
      (rfoldr rcons rb ra))

   ;; O(n log n)
   (define (list->rlist lst)
      (foldr rcons null lst))

   ;; O(n)
   (define (rlist->list rl)
      (rfoldr cons null rl))

   '(begin
      (print " => " (fold - 0 (lrange 0 1 100)))
      (print " => " (rfold - 0 (foldr rcons null (lrange 0 1 100))))
      (print " => " (foldr - 0 (lrange 0 1 100)))
      (print " => " (rfoldr - 0 (foldr rcons null (lrange 0 1 100))))
      (print " => " (let ((a (lrange 0 1 100)) (b (lrange 100 1 200)))
                        (equal? (rlist->list (rapp (list->rlist a) (list->rlist b)))
                                (rlist->list (list->rlist (append a b))))))
      (print " => " (let ((l (lrange 0 1 100))) (equal? (length l) (rlen (list->rlist l)))))
      (print " => " (let ((l (lrange 0 1 1000))) (equal? (length l) (rlen (list->rlist l)))))
      (print " => " (let ((l (lrange 0 1 10000))) (equal? (length l) (rlen (list->rlist l)))))
      )

   (define (rlist->num rl)
      (let loop ((rl rl) (n 1))
         (cond
            ((null? rl) null)
            ((eq? (type rl) 14)
               (cons n (loop (ref rl 2) n)))
            (else
               (cons n (loop (ref rl 2) (* n 2)))))))

   '(for-each
      (λ (n)
         ;(print " -> " (foldr rcons null (lrange 0 1 n)))
         (print " ==> " (rlist->num (foldr rcons null (lrange 0 1 n)))))
      (lrange 1 1 10))

)

|#
