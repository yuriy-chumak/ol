; http://rosettacode.org/wiki/Sorting_algorithms/Tree_sort_on_a_linked_list#Ol

(define (tree-sort l)
   (map car (ff->list
      (fold (lambda (ff p)
               (put ff p #t))
         #empty l))))

(print (tree-sort '(5 3 7 9 1)))
