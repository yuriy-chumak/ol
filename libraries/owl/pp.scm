; par, por, par*, por* compatibility layer
(define-library (owl pp)
   (export par por)
   (import
      (scheme base)
      (otus async))
(begin
   (import (owl io))
   ; compatibility layer, naive implementation for now
   (define (par . args)
      (map (lambda (arg)
            (if (procedure? arg)
               (case (await (async-linked arg))
                  (['done val]
                     val))
               arg))
         args))

   ; usable por function
   (define (por . args)
      (define main (async (lambda ()
         (let*((envelope (wait-mail))
               (sender msg envelope))
            msg))))
      (link main)

      ; run threads
      (define threads (map (lambda (thunk)
            (if (procedure? thunk)
               (async (lambda ()
                  (mail main (thunk))))
            else
               (mail main ['done thunk])
               #f))
         args))

      (define r (await main))
      ;; (map bell threads) ; wake up all threads (they can be sleeping)
      (map kill threads) ; kill em all!

      ; return result
      (ref r 2))

))
