; http://www.rosettacode.org/wiki/Greatest_element_of_a_list

(define x '(1 2 3 4 5))
(for-each print (list
   (max 1 2 3 4 5) ; 5
   (apply max x) ; 5
   (fold max (car x) x) ; 5
   (fold (lambda (a b)
            (if (less? a b) b a))
      (car x) x) ; 5
))
   