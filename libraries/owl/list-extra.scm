(define-library (owl list-extra)

   (export 
      lref lset ldel
      led ledn lins
      range ; iota
      take drop ; * srfi 1
      ; todo: add lazy range in srfi-41 as `stream-range`
      repeat
      split ;; lst n → head tail

      ; don't delete these legacy functions:
      lrange ; legacy `range` with uncommon arguments order
      )

   (import
      (scheme core)
      (owl math)
      (owl list)
      (only (srfi 1) take drop))

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

      ; fixme, iotas should be unfolds

      (define (range-up p s e)
         (if (< p e)
            (cons p (range-up (+ p s) s e))
            #null))

      (define (range-down p s e)
         (if (> p e)
            (cons p (range-down (+ p s) s e))
            #null))

      ; legacy `range` with uncommon arguments order
      (define (lrange from step to)
         (cond
            ((> step 0)
               (if (< to from) #null (range-up from step to)))
            ((< step 0)
               (if (> to from) #null (range-down from step to)))
            ((= from to) 
               null)
            (else 
               (runtime-error "bad lrange: " (list 'lrange from step to)))))

      (define range
         (case-lambda
            ((from to)
               (if (< to from) #null (range-up from 1 to)))
            ((from to step)
               (cond
                  ((> step 0)
                     (if (< to from) #null (range-up from step to)))
                  ((< step 0)
                     (if (> to from) #null (range-down from step to)))
                  (else
                     (runtime-error "bad range: " from to step))))))

      ; TODO: move to srfi-1
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
