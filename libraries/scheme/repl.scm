(define-library (scheme repl)
   (export 
      interaction-environment
      defined?
      env-get) ; env-get from (lang env)

   (import
      (scheme core)
      (lang env))

(begin

   (define-syntax interaction-environment
      (syntax-rules (*toplevel*)
         ((interaction-environment)
            *toplevel*)))

   (define-syntax defined?
      (syntax-rules (interaction-environment)
         ((defined? symbol)
            (env-get (interaction-environment) symbol #false))))

))
