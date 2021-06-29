(define-library (owl list-extra)

   (export 
      lref lset ldel
      led ledn lins
      take drop lrange; iota
      repeat
      split ;; lst n → head tail
      )

   (import
      (scheme core)
      (owl math)
      (owl list)
      (owl async)) ;?

   (begin
      (define lref list-ref)
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define (lset lst pos val)
         (cond
            ((null? lst) (runtime-error "list-set: out of list setting " val))
            ((eq? pos 0) (cons val (cdr lst)))
            (else
               (cons (car lst)
                  (lset (cdr lst) (- pos 1) val)))))
      
      (define (ldel lst pos)
         (cond
            ((null? lst) (runtime-error "list-del: out of list, left " pos))
            ((eq? pos 0) (cdr lst))
            (else (cons (car lst) (ldel (cdr lst) (- pos 1))))))

      ;; list edit node - apply op to list (not element) at pos pos, allowing last null
      (define (ledn lst pos op)
         (cond
            ((eq? pos 0) (op lst))
            ((null? lst) (runtime-error "ledn: out of list, remaining " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (ledn tl (- pos 1) op))))))
         
      ;; list edit - apply op to value at given pos
      (define (led lst pos op)
         (cond
            ((null? lst) (runtime-error "led: out of list, remaining " pos))
            ((eq? pos 0) (cons (op (car lst)) (cdr lst)))
            (else
               (lets ((hd tl lst))
                  (cons hd (led tl (- pos 1) op))))))

      ;; insert value to list at given position
      (define (lins lst pos val)
         (cond
            ((eq? pos 0) (cons val lst))
            ((null? lst) (runtime-error "lins: out of list, left " pos))
            (else
               (lets ((hd tl lst))
                  (cons hd (lins tl (- pos 1) val))))))

      ; take at n (or less) elemts from list l

      (define (take l n)
         (cond	
            ((eq? n 0) null)
            ((null? l) null)
            (else (cons (car l) (take (cdr l) (- n 1))))))

      ; drop n elements (or less) from list l

      (define (drop l n)
         (cond
            ((eq? n 0) l)
            ((null? l) l)
            (else (drop (cdr l) (- n 1)))))

      ; fixme, iotas should be unfolds

      (define (range-up p s e)
         (if (< p e)
            (cons p (range-up (+ p s) s e))
            null))

      (define (range-down p s e)
         (if (> p e)
            (cons p (range-down (+ p s) s e))
            null))

      (define (lrange from step to)
         (cond
            ((> step 0)
               (if (< to from) null (range-up from step to)))
            ((< step 0)
               (if (> to from) null (range-down from step to)))
            ((= from to) 
               null)
            (else 
               (runtime-error "bad lrange: " (list 'lrange from step to)))))

      (define (repeat thing n)
         (make-list n thing))

      (define (split l n)  
         (let loop ((l l) (o null) (n n))
            (cond
               ((null? l)
                  (values (reverse o) l))
               ((eq? n 0)
                  (values (reverse o) l))
               (else 
                  (loop (cdr l) (cons (car l) o) (- n 1))))))

))
