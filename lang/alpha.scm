;;;
;;; Alpha conversion
;;;

; convert all variables to fresh symbols. makes compilation easier.
; '(lambda (x) (lambda (x) x)) => (lambda (x:0) (lambda (x:1) x:1))


(define-library (lang alpha)
   (export alpha-convert)

   (import
      (r5rs core)
      (lang gensym)
      (lang ast)
      (owl math)
      (owl list)
      (owl list-extra)
      (owl ff))

   (begin
      (define (ok exp env) (tuple 'ok exp env))
      (define (fail reason) (tuple 'fail reason))

      (define (gensyms free n)
         (if (= n 0)
            (values null free)
            (lets ((gens next (gensyms (gensym free) (- n 1))))
               (values (cons free gens) next))))

      (define (alpha-list alpha exps env free)
         (if (null? exps) 
            (values null free)
            (lets
               ((this free (alpha (car exps) env free))
                (tail free (alpha-list alpha (cdr exps) env free)))
               (values (cons this tail) free))))

      (define (alpha exp env free)
         (tuple-case exp
            ((var sym)
               (values (mkvar (getf env sym)) free))
            ((call rator rands)
               (lets
                  ((rator free (alpha rator env free))
                   (rands free (alpha-list alpha rands env free)))
                  (values (mkcall rator rands) free)))
            ((lambda formals body)
               (lets
                  ((new-formals free (gensyms free (length formals)))
                   (body free
                     (alpha body
                        (for env (zip cons formals new-formals)
                           (λ (env node)
                              (put env (car node) (cdr node))))
                        free)))
                  (values (mklambda new-formals body) free)))
            ((lambda-var fixed? formals body) ;; <- mostly clone branch to be merged later
               (lets
                  ((new-formals free (gensyms free (length formals)))
                   (body free
                     (alpha body
                        (for env (zip cons formals new-formals)
                           (λ (env node)
                              (put env (car node) (cdr node))))
                        free)))
                  (values (tuple 'lambda-var fixed? new-formals body) free)))
            ((value val)
               (values exp free))
            ((values vals)
               (lets ((vals free (alpha-list alpha vals env free)))
                  (values (tuple 'values vals) free)))
            ((values-apply from to)
               (lets
                  ((from free (alpha from env free))
                   (to free   (alpha to   env free)))
                  (values (tuple 'values-apply from to) free)))
            ((ifeq a b then else)
               (lets
                  ((a free (alpha a env free))
                   (b free (alpha b env free))
                   (then free (alpha then env free))
                   (else free (alpha else env free)))
                  (values
                     (tuple 'ifeq a b then else)
                     free)))
            ((either then else)
               (lets
                  ((then free (alpha then env free))
                   (else free (alpha else env free)))
                  (values (tuple 'either then else) free)))
            (else
               (runtime-error "alpha: unknown AST node: " exp))))

      (define (alpha-convert exp env)
         (lets 
            ((exp free 
               (alpha exp empty (gensym exp))))
            (ok exp env)))
))
