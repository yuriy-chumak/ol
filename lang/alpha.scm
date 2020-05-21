;;;
;;; Alpha conversion
;;;

; convert all variables to fresh symbols. makes compilation easier.
; '(lambda (x) (lambda (x) x)) => (lambda (x:0) (lambda (x:1) x:1))


(define-library (lang alpha)
   (export alpha-convert)

   (import
      (scheme core)
      (lang gensym)
      (lang ast)
      (owl math)
      (owl list)
      (owl list-extra)
      (owl ff))

   (begin
      (define (ok exp env) ['ok exp env])
      (define (fail reason) ['fail reason])

      (define (gensyms free n)
         (if (eq? n 0)
            (values null free)
            (let*((gens next (gensyms (gensym free) (- n 1))))
               (values (cons free gens) next))))

      (define (alpha-list alpha exps env free)
         (if (null? exps) 
            (values null free)
            (let*((this free (alpha (car exps) env free))
                  (tail free (alpha-list alpha (cdr exps) env free)))
               (values (cons this tail) free))))

      (define (alpha exp env free)
         (case exp
            (['var sym]
               (values (mkvar (getf env sym)) free))
            (['call rator rands]
               (let*((rator free (alpha rator env free))
                     (rands free (alpha-list alpha rands env free)))
                  (values (mkcall rator rands) free)))
            (['lambda formals body]
               (let*((new-formals free (gensyms free (length formals)))
                     (body free (alpha body
                                 (fold (λ (env old new)
                                          (put env old new))
                                    env formals new-formals)
                                 free)))
                  (values (mklambda new-formals body) free)))
            (['lambda-var fixed? formals body] ;; <- mostly clone branch to be merged later
               (let*((new-formals free (gensyms free (length formals)))
                     (body free (alpha body
                                 (fold (λ (env old new)
                                          (put env old new))
                                    env formals new-formals)
                                 free)))
                  (values ['lambda-var fixed? new-formals body] free)))
            (['value val]
               (values exp free))
            (['values vals]
               (let*((vals free (alpha-list alpha vals env free)))
                  (values ['values vals] free)))
            (['values-apply from to]
               (let*((from free (alpha from env free))
                     (to free   (alpha to   env free)))
                  (values ['values-apply from to] free)))
            (['ifeq a b then else]
               (let*((a free (alpha a env free))
                     (b free (alpha b env free))
                     (then free (alpha then env free))
                     (else free (alpha else env free)))
                  (values
                     ['ifeq a b then else]
                     free)))
            (['brae then else]
               (let*((then free (alpha then env free))
                     (else free (alpha else env free)))
                  (values ['brae then else] free)))
            (else
               (runtime-error "alpha: unknown AST node: " exp))))

      (define (alpha-convert exp env)
         (let*((exp free (alpha exp empty (gensym exp))))
            (ok exp env)))
))
