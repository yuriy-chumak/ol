;;;
;;; Owl math module, things after basic arithmetic
;;;

; todo - http://www.math.dartmouth.edu/~carlp/PDF/complexity12.pdf

(define-library (owl math-extra)

   (export
      isqrt iroot iexpt
      ilog               ;; integer log a b

      expt expt-mod ** ^
      ncr npr
      !
      dlog dlog-simple
      fib
      histogram
      bisect
      ; inv-mod mod-solve

      ; r7rs
      exact-integer-sqrt ;; n → m r, m^2 + r = n
      rationalize
      )

   (import
      (scheme core)
      (scheme list)
      (owl math)
      (owl math fp)
      (owl iff)
      (owl list-extra)
      (owl sort)
      (only (otus async) por por*)
      (owl ff))

   (begin
      (define ncar car)
      (define ncdr cdr)

      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ;;;
      ;;; SQUARE ROOTS (stub)
      ;;;
      ; fixme, did not find a good integer sqrt algorithm which would
      ; work with these numerals, so i rolled my own as a quick substitute
      ; bench later

      ; Величина  положительного числа в битах (f - сколько бит добавить, типа "перенос")
      (define (nbits n f)
         (cond
            ((eq? n 0) f)
            ((eq? (type n) type-enum+)
               (lets ((hi lo (vm:shr n 1)))
                  (nbits hi (nat+1 f))))
            (else
               (let ((tl (cdr n)))
                  (if (null? tl)
                     (nbits (car n) f)
                     (nbits tl (add f (vm:vsize))))))))

      ; приблизительное значение корня "для затравки"
      (define (isqrt-init n)
         (lets
            ((nb (nbits n 0))
             (val (<< 1 (sub (>> nb 1) 1))))
            (if (eq? (band nb 1) 0)
               val
               (lets ((val2 (<< val 1)) (sq (mul val2 val2)))
                  (if (<= sq n) val2 val)))))

      (define (isqrt-fix hi bit n)
         (if (eq? bit 0)
            hi
            (lets ((this (bor hi bit)) (mid (mul this this)))
               (if (> mid n)
                  (isqrt-fix hi (>> bit 1) n)
                  (isqrt-fix this (>> bit 1) n)))))

      ; largest m where m^2 <= n
      (define (isqrt n)
         (cond
            ((eq? (type n) type-enum-) (negate (isqrt (negate n))))
            ((eq? (type n) type-int-) (negate (isqrt (negate n))))
            ((eq? n 0) 0)
            ((eq? n 1) 1)
            (else
               (let ((hi (isqrt-init n)))
                  (isqrt-fix hi (>> hi 1) n)))))

      (define (exact-integer-sqrt n)
         (let ((sq (isqrt n)))
            (values sq (sub n (mul sq sq)))))

      (assert (let*((x y (exact-integer-sqrt 17))) (list x y))  ===> '(4 1))

      ;; ------------------------------------
      ;; Bisect
      (define (bisect-fini op lo hi pos last)
         (cond
            ((= pos hi) last)
            ((op pos)
               (let ((next (+ pos 1)))
                  (cond
                     ((= next hi) pos)
                     ((op next)
                        (bisect-fini op lo hi (+ next 1) pos))
                     (else
                        pos))))
            ((= pos lo)
               #false)
            (else
               (bisect-fini op lo hi (- pos 1) last))))

      ; find the match or it's close neighbour by halving jump to correct direction
      (define (bisect-seek op lo hi pos step last)
         (cond
            ((eq? step 1)
               (bisect-fini op lo hi pos last))
            ((op pos)
               (bisect-seek op lo hi (+ pos step) (>> step 1) pos))
            (else
               (bisect-seek op lo hi (- pos step) (>> step 1) last))))

      ; search the last position in [lo ... hi-1] where op(x) is true
      (define (bisect op lo hi)
         (when (< lo hi)
            (let*((range (- hi lo))
                  (step (max 1 (>> range 1))))
               (bisect-seek op lo hi
                  (+ lo step) ;; move to middle of range
                  (max 1 (>> step 1)) ;; quarter step
                  #false))))


      ;;; exponentiation
      ; the usual O(lg n) exponentiation

      (define (expt-loop ap p out)
         (cond
            ((eq? p 0) out)
            ((eq? (band p 1) 0)
               (expt-loop (* ap ap) (>> p 1) out))
            (else
               (expt-loop (* ap ap) (>> p 1) (* out ap)))))

      (define (iexpt a p)
         (cond
            ((eq? p 0) 1)
            ((eq? a 1) 1)
            (else
               (expt-loop a (- p 1) a))))

      (define (iroot i n)
         (cond
            ((eq? i 0) 0)
            ((eq? i 1) 1)
            ((eq? n 1) i)
            ((negative? i)
               (complex 0 (iroot (* i -1) n)))
            (else
               (or
                  (bisect
                     (lambda (q) (<= (iexpt q n) i))
                     1 i)
                  1))))

      (define (expt a b)
         (cond
            ((eq? b 0) 1)
            ((eq? b 1) a)
            ((eq? b 2) (mul a a))
            ((eq? (type a) type-inexact) (fexpt a b))
            ((eq? (type b) type-enum+) (expt-loop a (sub b 1) a))
            ((eq? (type b) type-int+) (expt-loop a (sub b 1) a))
            ((eq? (type b) type-enum-) (/ 1 (expt a (negate b))))
            ((eq? (type b) type-int-) (/ 1 (expt a (negate b))))
            ((eq? (type b) type-rational) ;; todo: inexact if cannot be solved exactly
               (expt (iroot a (ref b 2)) (ref b 1)))
            ((eq? (type b) type-inexact) (fexpt a b))
            (else
               (big-bad-args 'expt a b))))

      ; (mod (expt a b) m) = (expt-mod a b m)

      (define (expt-mod-loop ap p out m)
         (cond
            ((eq? p 0) (mod out m))
            ((eq? (band p 1) 0)
               (expt-mod-loop (rem (mul ap ap) m) (>> p 1) out m))
            (else
               (expt-mod-loop (rem (mul ap ap) m) (>> p 1)
                  (rem (mul out ap) m) m))))

      (define (expt-mod a b m)
         (cond
            ((eq? b 0) (mod 1 m))
            ((eq? b 1) (mod a m))
            (else
               (expt-mod-loop (rem a m) (sub b 1) a m))))

      ;;;
      ;;; UNSORTED
      ;;;

      ; naive factorial

      (define (fact-iter n o)
         (if (eq? n 1)
            o
            (fact-iter (- n 1) (* o n))))

      (define (! n)
         (if (eq? n 0)
            1
            (fact-iter n 1)))

      ;;; npr, number of permutations, naively n!/(n-m)!

      (define (npr-loop n m o)
         (if (eq? m 0)
            o
            (npr-loop (- n 1) (- m 1) (* o n))))

      (define (npr n m)
         (if (eq? m 0)
            0
            (npr-loop (- n 1) (- m 1) n)))

      ;;; ncr, number of combinations, n choose m, simply n!/(m!(n-m)!)

      (define (ncr n m)
         (if (< n m)
            0
            (let ((mp (- n m)))
               (cond
                  ((eq? m 0) 1)
                  ((eq? mp 0) 1)
                  ((> m mp) (ncr n mp))
                  (else (/ (npr n m) (! m)))))))


      ;;;
      ;;; Discrete Logarithm
      ;;;

      ;; find ? such that (expt-mod a ? n) = y

      (define (dlp-naive y a n)
         (let loop ((x 0) (seen empty))
            (let ((this (expt-mod a x n)))
               (cond
                  ((= y this) x)
                  ((iget seen this #false) #false) ; looped, not solvable
                  (else (loop (+ x 1) (iput seen this #true)))))))

      ;; like naive, but avoids useless multiplications and remainders
      (define (dlp-simple y a n)
         (let loop ((x 0) (v 1) (seen empty))
            (cond
               ((>= v n) (loop x (rem v n) seen))      ; overflow
               ((= v y) x)                             ; solved
               ((iget seen v #false) #false)             ; looped -> not solvable
               (else                                   ; try next
                  (loop (+ x 1) (* v a) (iput seen v v))))))

      ;; like simple, but has O(1) space at the cost of ~1.5x time
      (define (dlp-th-step v a n)
         (let ((v (* a v)))
            (if (>= v n) (rem v n) v)))

      (define (dlp-th y a n)
         (if (= y 1)
            0
            (let loop ((x1 0) (v1 1) (x2 1) (v2 a) (step? #false))
               (cond
                  ((= v2 y) x2)                          ; hare finds carot \o/
                  ((= v1 v2) #false)                     ; hare finds tortoise o_O
                  (step?                                 ; fast hare is fast
                     (loop x1 v1 (+ x2 1) (dlp-th-step v2 a n) #false))
                  (else                                  ; enhance
                     (loop
                        (+ x1 1) (dlp-th-step v1 a n)
                        (+ x2 1) (dlp-th-step v2 a n) #true))))))


      ;; Shanks' baby-step giant-step algorithm (still not quite working properly)

      (define (carless a b) (< (car a) (car b)))

      (define (find-match b g pred)
         (cond
            ((null? b) #false)
            ((null? g) #false)
            ((= (caar b) (caar g))
               (let ((x (- (cdar g) (cdar b))))
                  (if (pred x)
                     x
                     (find-match (cdr b) (cdr g) pred))))
            ((< (caar b) (caar g)) (find-match (cdr b) g pred))
            (else (find-match b (cdr g) pred))))

      ;; a silly mod to avoid some remainders
      (define (bound x n)
         (if (< x n) x (mod x n)))

      ;; this can be done much more efficiently incrementally, but just testing for correctness now
      ;; todo: use incremental construction and an iff to check for matches

      (define (sqrt-ceil n)
         (let ((s (isqrt n)))
            (if (< (* s s) n)
               (+ s 1)
               s)))

      ;; y a n → x, such that y = a^x (mod n)
      (define (dlog-shanks y a n)
         (lets
            ((s (sqrt-ceil n))
             (baby
               (sort carless
                  (map (λ (r) (cons (rem (* y (expt-mod a r n)) n) r)) ; (ya^r. r)
                     (lrange 5 1 s)))
               ;(sort carless
               ;   (let loop ((ya (bound y n)) (r 0))
               ;      (if (= r s)
               ;         null
               ;         (cons (cons ya r) (loop (bound (* ya a) n) (+ r 1))))))
               )
             (giant
               (sort carless
                  (map (λ (t) (cons (expt-mod a (* s t) n) (bound (* t s) n)))
                     (lrange 1 1 (+ s 1))))))
            ;; i thought the match would be unique, but there seem to be many and some aren't solutions. not sure yet why.
            (find-match baby giant (λ (x) (= y (expt-mod a x n))))))

      (define dlog-simple dlp-th) ;; a simple reference implementation

      (define dlog dlog-shanks)


      ;;; Fibonacci numbers

      ;; n → f_n, f_n+1
      (define (fibs n)
         (cond
            ((eq? n 0) (values 1 1))
            ((eq? n 1) (values 1 2))
            (else
               (lets
                  ((a b (fibs (- (>> n 1) 1)))
                   (c (+ a b))
                   (aa (* a a)) (bb (* b b)) (cc (* c c)))
                  (if (eq? 0 (band n 1))
                     (values (+ aa bb) (- cc aa))
                     (values (- cc aa) (+ bb cc)))))))

      ;; one of the the relatively fast ways to compute fibonacci numbers
      (define (fib n)
         (if (< n 2)
            n
            (lets ((n sn (fibs (- n 1)))) n)))

      ;; (num ...) [n-bins] -> ((n-in-bin . bin-limit) ...)
      (define (histogram data . bins)
         (if (null? data)
            null
            (lets
               ((l (length data))
                (bins
                  (if (null? bins)
                     (min l (+ 1 (ilog2 l)))
                     (car bins)))
                (data (sort < data))
                (low (car data))
                (high (fold (λ (last next) next) low data))
                (bin (/ (- high low) bins)))
               (let loop ((data data) (count 0) (limit (+ low bin)))
                  (cond
                     ((null? data)
                        (list (cons count limit)))
                     ((> (car data) limit)
                        (cons (cons count limit)
                           (loop data 0 (+ limit bin))))
                     (else
                        (loop (cdr data) (+ count 1) limit)))))))

   (define rationalize
      ; Alan Bawden's algorithm
      (letrec
         ((rat1 ; x < y
         (lambda (x y)
            (cond
               ((> x 0) (rat2 x y))
               ((< y 0) (- (rat2 (- y) (- x))))
               (else (if (and (exact? x) (exact? y)) 0 0.0)))))
         (rat2 ; 0 < x < y
         (lambda (x y)
            (let ((fx (floor x)) (fy (floor y)))
               (cond
                  ((= fx x) fx)
                  ((= fx fy) (+ fx (/ (rat2 (/ (- y fy)) (/ (- x fx))))))
                  (else (+ fx 1)))))))
         (lambda (x e)
            (unless (real? x) (runtime-error 'rationalize x))
            (unless (real? e) (runtime-error 'rationalize e))
            (let ((x (- x e)) (y (+ x e)))
            (cond
               ((< x y) (rat1 x y))
               ((< y x) (rat1 y x))
               (else x))))))

   (define ** expt)
   (define ^ **)
))
