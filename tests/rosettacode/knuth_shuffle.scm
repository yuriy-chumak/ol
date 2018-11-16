; http://www.rosettacode.org/wiki/Knuth_shuffle
(define *include-dirs* (cons "tests/rosettacode" *include-dirs*))
(import (otus random!))

(define items (tuple 1 2 3 4 5 6 7 8 9))

(print "tuple before: " items)
(define (shuffle tp)
   (let ((items (vm:cast tp (type tp))))
      (for-each (lambda (i)
            (let ((a (ref items i))
                  (j (+ 1 (rand! i))))
               (set-ref! items i (ref items j))
               (set-ref! items j a)))
         (reverse (iota (size items) 1)))
      items))
(print "tuple after: " (shuffle items))

(define items (list 1 2 3 4 5 6 7 8 9))
(print "list before: " items)
(define (list-shuffle tp)
   (map (lambda (i)
         (list-ref tp i))
      (tuple->list
         (shuffle (list->tuple (iota (length tp)))))))

(print "list after: " (list-shuffle items))

