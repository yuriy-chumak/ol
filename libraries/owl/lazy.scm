;;;
;;; Lazy lists (poor man's streams)
;;;


;; in owl a lazy list some nodes of which may be thunks that evaluate to lists or lazy lists

(define-library (owl lazy)

   (export
      lfold lfoldr lmap lappend    ; main usage patterns
      lfor liota liter lnums
      lzip ltake llast llen
      lcar lcdr ledit
      ldrop llref
      lfor-each
      pair tail uncons
      force-ll                ; ll -> list
      subsets permutations    ; usual applications
      lunfold
      avg
      )

   (import
      (scheme core)
      (owl math)
      (owl list)
      (owl list-extra))

   (begin
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      ;; possibly delay construction of tail
      (define-syntax pair
         (syntax-rules ()
            ((pair a b) (cons a (delay b)))))

      (define (tail l)
         (cond
            ((pair? l) (cdr l))
            ((null? l) (runtime-error "tail: null stream " l))
            (else (tail (l)))))

      (define (llast l)
         (cond
            ((pair? l)
               (let ((tl (cdr l)))
                  (if (null? tl) (car l) (llast tl))))
            ((null? l) (runtime-error "llast: empty list: " l))
            (else (llast (l)))))

      ;; l → hd l' | error
      (define (uncons l d)
         (cond
            ((pair? l) (values (car l) (cdr l)))
            ((null? l) (values d l))
            (else (uncons (l) d))))

      (define (lfold op state lst)
         (cond
            ((pair? lst)
               (lfold op (op state (car lst)) (cdr lst)))
            ((null? lst) state)
            (else (lfold op state (lst)))))

      ; only swaps argument order, useful for making folds out of iterators
      (define (lfoldr op state lst)
         (cond
            ((pair? lst) (lfoldr op (op (car lst) state) (cdr lst)))
            ((null? lst) state)
            (else (lfoldr op state (lst)))))

      (define (llen lst) (lfold (lambda (n x) (+ n 1)) 0 lst))

      ; much more readable this way...

      (define (lfor state lst op) (lfold op state lst))

      ;; map, preserves laziness
      (define (lmap fn l)
         (cond
            ((pair? l)
               (cons (fn (car l))
                  (lmap fn (cdr l))))
            ((null? l)
               null)
            (else
               (λ () (lmap fn (l))))))

      ;; preserves laziness
      (define (lappend a b)
         (cond
            ((pair? a)
               (cons (car a) (lappend (cdr a) b)))
            ((null? a) b)
            (else
               (λ () (lappend (a) b)))))

      (define (lunfold op st end?)
         (if (end? st)
            null
            (lets ((this st (op st)))
               (pair this
                  (lunfold op st end?)))))

      (define (ltail) null)

      (define (lfor-each op lst)
         (cond
            ((pair? lst)
               (op (car lst))
               (lfor-each op (cdr lst)))
            ((not (null? lst))
               (lfor-each op (force lst)))))


      ;;; numbers (integers)

      (define (lnums-other n)
         (pair n (lnums-other (+ n 1))))

      (define (lnums-fix a)
         (if (less? a #xfff0)
            (lets
               ((b _ (vm:add a 1))
                (c _ (vm:add b 1))
                (d _ (vm:add c 1)))
               (ilist a b c (lambda () (lnums-fix d))))
            (lnums-other a)))

      (define (lnums n)
         (case (type n)
            (type-fix+ (lnums-fix n))
            (else (lnums-other n))))

      ;;; lazy lrange, with some fixnum hacks to make it run at decent speed

      (define (liota-walk st step end)
         (if (= st end)
            null
            (pair st (liota-walk (+ st step) step end))))

      (define liota-steps 8)

      (define (liota-walk-one st end)
         (if (= st end)
            null
            (cons st
               (let ((st (+ st 1)))
                  (if (= st end)
                     null
                     (pair st (liota-walk-one (+ st 1) end)))))))

      ; fixnum range lrange making 2 cells at a time. this is actually a bit
      ; faster than a corresponding (ugly) local loop.

      (define (liota-fix pos end)
         (if (less? pos end)
            (lets ((posp u (vm:add pos 1)))
               (if (less? posp end)
                  (lets ((next o (vm:add posp 1)))
                     (cons pos (pair posp (liota-fix next end))))
                  (list pos)))
            null))

      (define (liota pos step end)
         (if (eq? step 1)
            (if (eq? (type pos) type-fix+)
               (if (eq? (type end) type-fix+)
                  (liota-fix pos end)         ; positive fixnum range interval
                  (liota-walk-one pos end))    ; increment lrange
               (liota-walk-one pos end))
            (liota-walk pos step end)))      ; general lrange

      (define (ledit op l)
         (cond
            ((pair? l)
               (let ((x (op (car l))))
                  (if x
                     (append x (ledit op (cdr l)))
                     (cons (car l) (ledit op (cdr l))))))
            ((null? l) l)
            (else 
               (lambda ()
                  (ledit op (l))))))

      (define (liter op st)
         (pair st (liter op (op st))))

      (define (ltaker l n o)
         (cond
            ((null? l) (reverse o))
            ((pair? l)
               (if (eq? n 0)
                  (reverse o)
                  (ltaker (cdr l) (- n 1) (cons (car l) o))))
            (else
               (ltaker (l) n o))))

      ;; l n → l | Null (if out of list)
      (define (ldrop l n)
         (cond
            ((eq? n 0) l)
            ((pair? l) (ldrop (cdr l) (- n 1)))
            ((null? l) l)
            (else (ldrop (l) n))))

      (define blank '(blank))

      (define (llref ll p)
         (lets
            ((ll (ldrop ll p))
             (val ll (uncons ll blank)))
            (if (eq? val blank)
               (runtime-error "llref: out of list: " p)
               val)))

      (define (ltake l n) (ltaker l n null))

      ;; zip, preserves laziness of first argument
      (define (lzip op a b)
         (cond
            ((null? a) null)
            ((null? b) null)
            ((pair? a)
               (if (pair? b)
                  (pair (op (car a) (car b))
                     (lzip op (cdr a) (cdr b)))
                  (lzip op a (b))))
            (else
               (λ () (lzip op (a) b)))))

      ; lst -> stream of (lst' ...)
      ; first == lst, changes mostly on the head of the list

      (define (lperm-take l out rest)
         (if (null? l)
            (cons out rest)
            (let loop ((a l) (b null))
               (if (null? a)
                  (rest)
                  (lperm-take (append b (cdr a)) (cons (car a) out)
                     (lambda () (loop (cdr a) (cons (car a) b))))))))

      (define (lperms l)
         (if (null? l)
            '(())
            (lperm-take (reverse l) null ltail)))


      (define permutations lperms)

      ; lst -> stream of (lst' ...)
      ; first == lst

      ;(define (ssubs-take l out more)
      ;   (if (null? l)
      ;      (cons out more)
      ;      (ssubs-take (cdr l) (cons (car l) out)
      ;         (lambda () (ssubs-take (cdr l) out more)))))
      ;
      ;(define (ssubs l)
      ;   (ssubs-take (reverse l) null ltail))

      (define (lpick l out n more)
         (cond
            ((eq? n 0) (cons out more))
            ((null? l) more)
            (else
               (lpick (cdr l) (cons (car l) out) (- n 1)
                  (lambda () (lpick (cdr l) out n more))))))

       ; subsets of growing size
      (define (subs l)
         (if (null? l)
            '(())
            (pair null
               (let ((end (+ (length l) 1)))
                  (let loop ((n 1))
                     (if (= n end)
                        null
                        (lpick l null n
                           (lambda ()
                              (loop (+ n 1))))))))))

      (define subsets subs)

      ; (lfold (lambda (n s) (print s) (+ n 1)) 0 (subsets (lrange 0 1 5)))

      ; (lfold (lambda (n s) (print s) (+ n 1)) 0 (permutations (lrange 0 1 5)))

      (define (force-ll it)
         (cond
            ((null? it) it)
            ((pair? it) (cons (car it) (force-ll (cdr it))))
            (else (force-ll (it)))))

      (define (avg ll)
         (let loop ((ll ll) (sum 0) (len 0))
            (cond
               ((null? ll)
                  (if (eq? len 0)
                     (runtime-error "avg: empty list: " ll)
                     (/ sum len)))
               ((pair? ll)
                  (loop (cdr ll) (+ sum (car ll)) (+ len 1)))
               (else
                  (loop (ll) sum len)))))

      (define (lcar ll) (if (pair? ll) (car ll) (lcar (ll))))

      (define (lcdr ll) (if (pair? ll) (cdr ll) (lcdr (ll))))

))
