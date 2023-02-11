(define-library (scheme repl)
   (export 
      interaction-environment

      ; *ol specific:
      defined?
   )

   (import
      (scheme core))
      ;(only (lang env) env-get))

(begin

   (define-lazy-macro (interaction-environment)
      current-environment)

   (define-lazy-macro (defined? symbol)
      (if (get current-environment symbol #false) #true))

))
