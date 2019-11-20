;;;
;;; Owl math module, prime and factoring arithmetic
;;;

(define-library (owl math-prime)

   (export 
      factor prime?
      primes-between divisor-count
      totient phi divisor-sum
      )

   (import 
      (scheme core)
      (owl math)
      (owl iff) 
      (owl list)
      (owl list-extra)
      (owl math-extra)
      (owl sort)
      (only (owl interop) por por*)
      (owl ff))

   (begin
      ;;;
      ;;; PRIMES AND FACTORING
      ;;;

      ;; primality testing - miller-rabin

      ; n < 9,080,191, a = 31 and 73.
      ; n < 4,759,123,141, a = 2, 7, and 61.
      ; n < 2,152,302,898,747, a = 2, 3, 5, 7, and 11.
      ; n < 3,474,749,660,383, a = 2, 3, 5, 7, 11, and 13.
      ; n < 341,550,071,728,321, a = 2, 3, 5, 7, 11, 13, and 17.

      (define first-primes
         (pairs->ff
            (map (λ (x) (cons x x))
               '(2 3 5 7 11 13 17))))

      ; divide by 2 (shift 1) while even and count shifts
      (define (miller-qk q k)
         (if (eq? (band q 1) 0)
            (miller-qk (>> q 1) (+ k 1))
            (values q k)))

      (define (miller-rabin n x)
         (lets ((q k (miller-qk (- n 1) 0)))
            (let loop ((y (expt-mod x q n)) (j 0))
               (cond
                  ((= j k) #false)
                  ((and (eq? j 0) (eq? y 1)) #true)
                  ((= y (- n 1)) #true)
                  ((and (> j 0) (= y 1)) #false)
                  (else (loop (expt-mod y 2 n) (+ j 1)))))))

      (define (miller-rabin-cases-ok? num tests)
         (fold
            (lambda (status a) (and status (miller-rabin num a)))
            #true tests))

      (define assume-riemann-hypothesis? #true)

      ; write n as 2^s*d by factoring out powers of 2 from n-1
      ; for all a in [2 .. min(n-1, floor(2*(ln n)^2))]
      ;      if a^d = 1 (mod n)
      ;         next a
      ;         loop r in [0, s-1]
      ;            if (a^(d<<r)) = n-1
      ;               next a
      ;             if out of r
      ;               return composite

      (define (factor-out-twos n)
         (let loop ((n n) (p 0))
            (if (eq? 0 (band n 1))
               (loop (>> n 1) (+ p 1))
               (values n p))))

      ; bound by using a rational approximation e-ish < e

      (define e-ish 25946/9545)   ; log e-ish = 0.999999998

      (define (ln+ n)   ; return a number >= floor(ln(n))
         (let loop ((a 1) (b 1) (p 0))
            (if (> (div a b) n)
               p
               (loop (* a 25946) (* b 9545) (+ p 1)))))

      (define (miller-rabin-det n)
         (lets
            ((np (- n 1))
             (d s (factor-out-twos np))
             (aover (min n (<< (expt (ln+ n) 2) 1))))
            (let loop ((a 2))
               (cond
                  ((= a aover) #true)
                  ((= 1 (expt-mod a d n)) (loop (+ a 1)))
                  (else
                     (let loopr ((r (- s 1)))
                        (cond
                           ((= r -1) #false)   ; composite 
                           ((= (expt-mod a (<< d r) n) np) (loop (+ a 1)))
                           (else (loopr (- r 1))))))))))

      (define (prime? n)
         (cond
            ((eq? n 1) #false)
            ((eq? n 2) #true)
            ((eq? 0 (band n 1)) #false)
            ((get first-primes n #false) #true)
            ((< n 1373653) (miller-rabin-cases-ok? n '(2 3)))
            ((< n 9080191) (miller-rabin-cases-ok? n '(31 73)))
            ((< n 4759123141) (miller-rabin-cases-ok? n '(2 7 61)))
            ((< n 2152302898747) (miller-rabin-cases-ok? n '(2 3 5 7 11)))
            ((< n 3474749660383) (miller-rabin-cases-ok? n '(2 3 5 7 11 13)))
            ((< n 341550071728321) (miller-rabin-cases-ok? n '(2 3 5 7 11 13 17)))
            (else (miller-rabin-det n))))

      ;; Atkin sieve 

      (define (atkin-flip ff num)
         (iput ff num (not (iget ff num #false))))

      (define (between? a x b)
         (cond
            ((> a x) #false)
            ((< b x) #false)
            (else #true)))

      ; later apply the knowledge about limits
      (define (atkin-candidates lo max)
         (let ((lim (isqrt max)))
            (let loox ((store #empty) (x 1))
               (if (> x lim)
                  store
                  (let looy ((store store) (y 1))
                     (if (> y lim)
                        (loox store (+ x 1))
                        ; eww, fix later
                        (lets
                           ((xx (* x x)) 
                            (yy (* y y))
                            (n (+ (* 4 xx) yy))
                            (nm (rem n 12))
                            (store
                              (if (and (between? lo n max) (or (eq? nm 1) (eq? nm 5)))
                                 (atkin-flip store n)
                                 store))
                            (n (+ (* 3 xx) yy))
                            (nm (rem n 12))
                            (store
                              (if (and (between? lo n max) (eq? nm 7))
                                 (atkin-flip store n)
                                 store))
                            (n (- n (<< yy 1))))
                           (if (and (> x y) 
                                 (and (between? lo n max) (eq? (rem n 12) 11)))
                              (looy (atkin-flip store n) (+ y 1))
                              (looy store (+ y 1))))))))))

      (define (atkin-remove-duplicates-of store prime max)
         (let ((xx (* prime prime)))
            (let loop ((store store) (val xx))
               (cond
                  ((> val max) store)
                  ((iget store val #false)
                     (loop (atkin-flip store val) (+ val xx)))
                  (else
                     (loop store (+ val xx)))))))

      (define (atkin-remove-squares max store)
         (ifold
            (lambda (store prime v)
               (if v (atkin-remove-duplicates-of store prime max) store))
            store store))

      (define (atkin-try pows prime)
         (let loop ((n (car pows)) (these 0))
            (if (eq? n 1)
               (if (eq? these 0)   
                  pows
                  (cons 1 (cons (cons prime these) (cdr pows))))
               (let ((q (ediv n prime)))
                  (cond
                     (q (loop q (+ these 1)))
                     ((eq? these 0) pows)
                     (else
                        (cons n    (cons (cons prime these) (cdr pows)))))))))
                        
      (define (atkin-apply store pows)
         (call/cc
            (lambda (done)
               (ifold
                  (lambda (out k v)
                     (let ((res (atkin-try out k)))
                        (if (eq? (car res) 1)
                           (done res)
                           res)))
                  pows store))))

      ;; primes in the range [lo .. hi] (inclusive)


      (define (atkin-primes-between lo hi)
         (cond
            ((> lo hi) null)
            ; 2 and 3 are special
            ((between? lo 2 hi) (cons 2 (atkin-primes-between 3 hi)))
            ((between? lo 3 hi) (cons 3 (atkin-primes-between 5 hi)))
            (else
               (sort <
                  (ifold 
                     (λ (out k v) (if v (cons k out) out))
                     null
                     (atkin-remove-squares hi
                        (atkin-candidates lo hi)))))))

      (define primes-between atkin-primes-between)

      (define (factor-atkin-between lo hi pows)
         (atkin-apply 
            (atkin-remove-squares hi
               (atkin-candidates lo hi))
            pows))

      (define (atkin-factor-driver pows lo)
         (let ((max (min (<< lo 1) (isqrt (car pows)))))
            (let ((pows (factor-atkin-between lo max pows)))
               (cond
                  ((eq? (car pows) 1)   
                     (cdr pows))
                  ((>= max (isqrt (car pows)))
                     (cons (cons (car pows) 1) (cdr pows)))
                  (else
                     (atkin-factor-driver pows  max))))))

      ; fixme, try different options
      ;   - factor out twos first
      ;   - try low primes 
      ;   - more low primes
      ;  - quick prime? check (maybe miller-rabin (2 3 5))
      ;  - limited pollard-rho
      ;   - full trial division
      ;   - intermediate prime? checks

      (define (factor n)   
         (if (> n 1)
            (por
               ;; prime check is relatively fast (for deterministic range) so try it first
               (if (prime? n)
                  (list (cons n 1))
                  #false)
               (let 
                  ((pows
                     (fold atkin-try (list n)   
                        '(2 3 5 7 11 13 17 19 23 29 31))))
                  (if (eq? (car pows) 1)
                     (cdr pows)
                     (atkin-factor-driver pows 32))))
            null))
            
            
      ; number of divisors of n, aka tau, sigma0, A000005
      (define (divisor-count n)
         (if (eq? n 1)
            1
            (fold 
               (lambda (out n) (* out (+ (cdr n) 1)))
               1 (factor n))))


      ; Euler's totient, aka phi

      ; phi(p) = p-1 when p is a prime
      ; phi(p^n) = (p-1) * p^(n-1)
      ; phi(ab) = phi(a) * phi(b) when gcd(a,b) = 1

      (define (totient n)
         (if (< n 2)
            1
            (fold
               (lambda (left factor) (- left (/ left (car factor))))
               n (factor n))))

      (define phi totient)


      ; sum of divisors of n, A000203

      (define (divisor-sum num)
         (if (eq? num 1)
            1
            (fold
               (lambda (total factor)
                  (* total
                     (/ (- (expt (car factor) (+ (cdr factor) 1)) 1)
                        (- (expt (car factor) 1) 1))))
               1 (factor num))))
            
))

