(import (etc suffix))
(define str "banana")

(for-each
   (λ (pos)
      (print (if (< pos 10) " " "") pos " -> " (substring str pos (string-length str))))
   (vector->list
      (suffix-array str)))
