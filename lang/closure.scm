;;;
;;; Convert lambdas to closures where necessary
;;;

(define-library (lang closure)

   (export 
      build-closures 
      uncompiled-closure?)

   (import
      (scheme base)
      (owl list)
      (lang ast)
      (owl math)
      (owl list-extra)
      (lang env)
      (lang primop)
      (only (owl io) print))

   (begin
      (define (ok exp env) ['ok exp env])
      (define (fail reason) ['fail reason])
      (define-syntax lets (syntax-rules () ((lets . stuff) (let* . stuff)))) ; TEMP

      (define (small-value? val)
         (or
            (eq? val #true)
            (eq? val #false)
            (and
               (eq? (type val) type-value+)
               (less? val 127))
            (eq? val #null)))

      (define (value-primop val)
         (and (vector? val)
            (eq? 'value (ref val 1))
            (primitive? (ref val 2))))

      (define (closurize-list closurize exps used)
         (if (null? exps)
            (values null used)
            (lets
               ((this used (closurize (car exps) used #true))
                (tail used (closurize-list closurize (cdr exps) used)))
               (values (cons this tail) used))))

      (define (closurize-call closurize rator rands used)
         (let ((op (value-primop rator)))
            (if op
            then
               (case (car rands)
                  (['lambda-var fixed? formals body]
                     (let*((cont used (closurize (car rands) used #false))
                           (rands used (closurize-list closurize (cdr rands) used)))
                        (values (mkcall rator (cons cont rands)) used)))
                  (['var name]
                     (let
                        ((dummy-cont
                           ;;; used only once and called immediately
                           (mklambda (list '| |); mock argument name
                              (mkcall (mkvar name)
                                 (list (mkvar '| |))))))
                        (closurize-call closurize rator
                           (cons dummy-cont (cdr rands))
                           used)))
                  (else
                     (runtime-error "Bad primitive continuation:" (car rands))))
            else
               (let*((rator used (closurize rator used #false))
                     (rands used (closurize-list closurize rands used)))
                  (values (mkcall rator rands) used)))))

      (define (closurize exp used close?)
         (case exp
            (['value val]
               (values exp used))
            (['var sym]
               (if (has? used sym)
                  (values exp used)
                  (values exp (cons sym used))))
            (['call rator rands]
               (closurize-call closurize rator rands used))
            (['lambda-var fixed? formals body]
               (lets
                  ((body bused
                     (closurize body null #true))
                   (clos (diff bused formals)))
                  (values
                     (if close?
                        ['closure fixed? formals body clos]
                        ['lambda-var fixed? formals body])
                     (union used clos))))
            (['ifeq a b then else]
               (lets
                  ((a used (closurize a used #true))
                   (b used (closurize b used #true))
                   (then used (closurize then used #true))
                   (else used (closurize else used #true)))
                  (values
                     ['ifeq a b then else]
                     used)))
            (['brae func else]
               ;; fixme: operator position handling of resulting unclosurized case-lambdas is missing
               (if close? 
                  ;; a normal value requiring a closure, and first node only 
                  (lets
                     ((func this-used (closurize func null #false)) ;; no used, don't close
                      (else this-used (closurize else this-used #false))) ;; same used, dont' close rest 
                     (values
                        ['closure-case ['brae func else] this-used]  ;; used the ones in here
                        (union used this-used)))                   ;; needed others and the ones in closure
                  ;; operator position case-lambda, which can (but isn't yet) be dispatche at compile 
                  ;; time, or a subsequent case-lambda node (above case requests no closurization) 
                  ;; which doesn't need to be closurized
                  (lets 
                     ((func used (closurize func used #false)) ;; don't closurize codes
                      (else used (closurize else used #false))) ;; ditto for the rest of the tail
                     (values 
                        ['brae func else]
                        used))))
            (else
               (runtime-error "closurize: unknown exp type: " exp))))

      (define (literalize-list literalize exps used)
         (if (null? exps)
            (values null used)
            (lets
               ((this used (literalize (car exps) used))
                (tail used (literalize-list literalize (cdr exps) used)))
               (values (cons this tail) used))))

      (define (literalize-call literalize rator rands used)
         (lets
            ((rator used 
               (if (value-primop rator)
                  (values rator used)
                  (literalize rator used)))
             (rands used 
               (literalize-list literalize rands used)))
            (values (mkcall rator rands) used)))

      (define closure-tag (list 'uncompiled-closure))

      (define (uncompiled-closure? thing)
         (and (pair? thing) (eq? (car thing) closure-tag)))

      (define (literalize exp used)
         (case exp
            (['value val]
               (values exp
                  (if (or (has? used val) (small-value? val))
                     used
                     (append used (list val)))))
            (['var sym]
               (values exp used))
            (['call rator rands]
               (literalize-call literalize rator rands used))
            (['lambda-var fixed? formals body]
               (lets ((body used (literalize body used)))
                  (values ['lambda-var fixed? formals body] used)))
            ; 
            (['closure fixed? formals body clos]
               ;; note, the same closure exp (as in eq?) is stored to both literals
               ;; and code. the one in code will be used to make instructions 
               ;; for closing it and the one in literals will be the executable 
               ;; part to close against.
               (let*((body bused (literalize body null))
                     (closure-exp ['closure fixed? formals body clos bused])
                     (used (append used (list (cons closure-tag closure-exp)))))
                  (values
                     ;;; literals will be #(header <code> l0 ... ln)
                     ['make-closure (+ 1 (length used)) clos bused]
                     ;; also literals are passed, since the closure type 
                     ;; and calling protocol are different depending on 
                     ;; whether there are literals
                     used)))

            (['closure-case body clos] ;; clone branch, merge later
               (lets
                  ((body bused (literalize body null))
                   (closure-exp ['closure-case body clos bused])
                   (used (append used (list (cons closure-tag closure-exp)))))
                  (values ['make-closure (+ 1 (length used)) clos bused] used)))
            (['ifeq a b then else]
               (lets
                  ((a used (literalize a used))
                   (b used (literalize b used))
                   (then used (literalize then used))
                   (else used (literalize else used)))
                  (values
                     ['ifeq a b then else]
                     used)))
            (['brae func else]
               (lets
                  ((func used (literalize func used))
                   (else used (literalize else used)))
                  (values ['brae func else] used)))
            (else
               (runtime-error "literalize: unknown exp type: " exp))))

      (define (build-closures exp env)
         (lets
            ((exp used (closurize exp null #true))
             (exp lits (literalize exp null)))
            (if (and (pair? lits) (uncompiled-closure? (car lits)))
               (ok (cdar lits) env)
               (runtime-error "Bad closurize output: " 
                  'exp exp 'lits lits))))

))

