(define-library (owl list)

   ; todo: move fold to srfi-1
   (export
      for ; deprecated
      getq last drop-while
      mem
      fold-map foldr-map ; deprecated
      keep remove
      all some
      smap unfold
      take-while                ;; pred, lst -> as, bs
      halve

      diff union intersect)

   (import
      (scheme core)
      (scheme list))

   (begin
      (define o (λ (f g) (λ (x) (f (g x))))) ; wtf???

      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ;; constants are always inlined, so you pay just one byte of source for readability
      (define null #null)

      (define-syntax withcc
         (syntax-rules ()
            ((withcc name proc)
               (call/cc (λ (name) proc)))))

      ; (for st l op) == (fold op st l)
      ; just usually less indentation clutter

      ; deprecated
      (define (for st l op)
         (if (null? l)
            st
            (for (op st (car l)) (cdr l) op)))

      (define (unfold op st end?)
         (if (end? st)
            null
            (lets ((this st (op st)))
               (cons this (unfold op st end?)))))


      (define (getq lst k)
         (cond
            ((null? lst) #false)
            ((eq? k (car (car lst))) (car lst))
            (else (getq (cdr lst) k))))

      (define (last l def)
         (fold (λ (a b) b) def l))

      (define (mem cmp lst elem)
         (cond
            ((null? lst) #false)
            ((cmp (car lst) elem) lst)
            (else (mem cmp (cdr lst) elem))))

      ;; misc

      (define (drop-while pred lst)
         (cond
            ((null? lst) lst)
            ((pred (car lst))
               (drop-while pred (cdr lst)))
            (else lst)))

      (define (take-while pred lst)
         (let loop ((lst lst) (taken null))
            (cond
               ((null? lst) (values (reverse taken) null))
               ((pred (car lst)) (loop (cdr lst) (cons (car lst) taken)))
               (else (values (reverse taken) lst)))))

      (define (keep pred lst)
         (foldr (λ (x tl) (if (pred x) (cons x tl) tl)) null lst))

      (define (remove pred lst)
         (keep (o not pred) lst))

      (define (all pred lst)
         (withcc ret
            (fold (λ (ok x) (if (pred x) ok (ret #false))) #true lst)))

      (define (some pred lst)
         (withcc ret
            (fold (λ (_ x) (let ((v (pred x))) (if v (ret v) #false))) #false lst)))

      ; map carrying one state variable down like fold
      (define (smap op st lst)
         (if (null? lst)
            null
            (lets ((st val (op st (car lst))))
               (cons val
                  (smap op st (cdr lst))))))


      ; could also fold

      (define (fold-map o s l)
         (let loop ((s s) (l l) (r null))
            (if (null? l)
               (values s (reverse r))
               (lets ((s a (o s (car l))))
                  (loop s (cdr l) (cons a r))))))

      (define (foldr-map o s l)
         (if (null? l)
            (values s null)
            (lets
               ((a (car l))
                (s l (foldr-map o s (cdr l))))
               (o a s))))


      (define (diff a b)
         (cond
            ((null? a) a)
            ((has? b (car a))
               (diff (cdr a) b))
            (else
               (cons (car a)
                  (diff (cdr a) b)))))

      (define (union a b)
         (cond
            ((null? a) b)
            ((has? b (car a))
               (union (cdr a) b))
            (else
               (cons (car a)
                  (union  (cdr a) b)))))

      (define (intersect a b)
         (cond
            ((null? a) null)
            ((has? b (car a))
               (cons (car a)
                  (intersect (cdr a) b)))
            (else
               (intersect (cdr a) b))))

      ;; lst → a b, a ++ b == lst, length a = length b | length b + 1
      (define (halve lst)
         (let walk ((t lst) (h lst) (out null))
            (if (null? h)
               (values (reverse out) t)
               (let ((h (cdr h)))
                  (if (null? h)
                     (values (reverse (cons (car t) out)) (cdr t))
                     (walk (cdr t) (cdr h) (cons (car t) out)))))))

))
