(import (lang eval))

;; normal sexp
(print (eval '(+ 1 2) (interaction-environment)))

;; linked code
(print (eval (list + 1 2) (interaction-environment)))

;; nested
(print (eval (list eval '(+ 1 2) (interaction-environment)) (interaction-environment)))

;; silly
(let ((add (fasl-encode +)))
   (eval (list 'print (list (list 'fasl-decode (list 'quote add) 42) 1 2)) (interaction-environment)))
