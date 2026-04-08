(define-library (scheme eval)
   (export 
      eval)

   (import
      (scheme core)
      (scheme repl)
      (prefix (otus eval) otus/)
      ; default Ol environment:
      (otus lisp))

(begin
   (define eval
      (define (eval expr env)
         (case (otus/eval expr env)
            (['ok expr env]
               expr)
            (['fail reason]
               (runtime-error "eval failed with" reason))
            (else
               (runtime-error "unknown eval fail error"))))
      (case-lambda
         ((expr env) (eval expr env))
         ((expr) (eval expr (interaction-environment)))))
))
