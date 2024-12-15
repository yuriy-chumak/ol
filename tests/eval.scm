(import (scheme eval))
(import (scheme repl))

;; normal sexp
(print (eval '(+ 1 2)))

;; normal sexp using env
(print (eval '(+ 1 2) (interaction-environment)))

;; linked code
(print (eval (list + 1 2)))

;; nested
(print (eval (list eval '(+ 1 2))))

;; silly
(let ((add (fasl-encode +)))
   (eval (list 'print (list (list 'fasl-decode (list 'quote add) 42) 1 2))))

;; error handling
(with-exception-handler
   (lambda (x)
      (print "error detected.\n  "
         (error-object-message x)))

   (lambda ()
      (print (eval '(not-a-func 1 2)))))
