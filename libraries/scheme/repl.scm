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

   (define-lazy-macro interaction-environment (lambda ()
      current-environment))

   (define-lazy-macro defined? (lambda (symbol)
      (if (get current-environment symbol #false) #true)))

))
