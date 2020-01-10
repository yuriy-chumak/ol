; http://rosettacode.org/wiki/A_star_search_algorithm

; level: list of lists, any except 1 means the cell is empty
; from: start cell in (x . y) mean
; to: destination cell in (x . y) mean
(define (A* level from to)
   (define (hash xy) ; internal hash
      (+ (<< (car xy) 16) (cdr xy)))

   ; naive test for "is the cell is empty?"
   (define (floor? x y)
      (let by-y ((y y) (map level))
         (cond
            ((< y 0) #true)
            ((null? map) #true)
            ((= y 0)
               (let by-x ((x x) (map (car map)))
                  (if (< x 0) #true
                  (if (null? map) #true
                  (if (= x 0)
                     (not (eq? (car map) 1))
                  (by-x (- x 1) (cdr map)))))))
            (else
               (by-y (- y 1) (cdr map))))))

   (unless (equal? from to) ; search not finished yet
      (let step1 ((n 999) ; maximal count of search steps
                  (c-list-set #empty)
                  (o-list-set (put #empty (hash from)  [from #f  0 0 0])))
         (unless (empty? o-list-set) ; do we have a space to move?
            ; no. let's find cell with minimal const
            (let*((f (ff-fold (lambda (s key value)
                                 (if (< (ref value 5) (car s))
                                    (cons (ref value 5) value)
                                    s))
                        (cons 9999 #f) o-list-set))
                  (xy (ref (cdr f) 1))
                  ; move the cell from "open" to "closed" list
                  (o-list-set (del o-list-set (hash xy)))
                  (c-list-set (put c-list-set (hash xy) (cdr f))))

               ;
               (if (or (eq? n 0)
                       (equal? xy to))
                  (let rev ((xy xy))
                     ; let's unroll the math and return only first step
                     (let*((parent (ref (get c-list-set (hash xy) #f) 2))
                           (parent-of-parent (ref (get c-list-set (hash parent) #f) 2)))
                        (if parent-of-parent (rev parent)
                           (cons ;[
                              (- (car xy) (car parent))
                              (- (cdr xy) (cdr parent))
                              ;c-list-set
                              ;o-list-set
                              ))))

                  (let*((x (car xy))
                        (y (cdr xy))
                        (o-list-set (fold (lambda (n v)
                                       (if (and
                                             (floor? (car v) (cdr v))
                                             (eq? #f (get c-list-set (hash v) #f)))
                                          (let ((G (+ (ref (get c-list-set (hash xy) #f) 3) 1)); G of parent + 1
                                                ; H calculated by "Manhattan method"
                                                (H (* (+ (abs (- (car v) (car to)))
                                                         (abs (- (cdr v) (cdr to))))
                                                      2))
                                                (got (get o-list-set (hash v) #f)))

                                             (if got
                                                (if (< G (ref got 3))
                                                   (put n (hash v)  [v xy  G H (+ G H)])
                                                   n)
                                                (put n (hash v)  [v xy  G H (+ G H)])))
                                          n))
                                       o-list-set (list
                                                      (cons x (- y 1))
                                                      (cons x (+ y 1))
                                                      (cons (- x 1) y)
                                                      (cons (+ x 1) y)))))
                     (step1 (- n 1) c-list-set o-list-set))))))))

; ...
(define level '(
   (1 1 1 1 1 1 1 1 1 1)
   (1 A 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 1 1 1 0 1)
   (1 1 0 0 0 0 0 1 0 1)
   (1 0 0 1 0 0 0 1 0 1)
   (1 0 0 1 1 1 1 1 0 1)
   (1 0 0 0 0 0 0 0 0 1)
   (1 0 0 0 1 0 0 0 B 1)
   (1 1 1 1 1 1 1 1 1 1)
))
(print "the map:")
(for-each print level)

; let's check that we can't move to (into wall)
(print "we should not reach the '(9 . 9) cell:")
(print (A* level '(1 . 1) '(9 . 9)))
(print "ok, we got #false, so really can't.")

(print "now try to reach cell '(8 . 8) - the 'B' point:")
(define to '(8 . 8))
(define (plus a b) (cons (+ (car a) (car b)) (+ (cdr a) (cdr b)))) ; helper

(define path
(let loop ((me '(1 . 1)) (path '()))
   (if (equal? me to)
      (begin
         (print "here I am!")
         (cons to path))
   (let ((move (A* level me to)))
      (if (not move)
         (begin
            (print "no way, sorry :(")
            #false)
         (let ((step (plus me move)))
            (print me " + " move " -> " step)
            (loop step (cons me path))))))))

; let's draw the path?
(define (has? lst x) ; helper
   (cond
      ((null? lst) #false)
      ((equal? (car lst) x) lst)
      (else (has? (cdr lst) x))))

(define solved
   (map (lambda (row y)
         (map (lambda (cell x)
               (cond
                  ((equal? (cons x y) '(1 . 1)) "A")
                  ((equal? (cons x y) '(8 . 8)) "B")
                  ((has? path (cons x y)) "*")
                  (else cell)))
            row (iota 10)))
      level (iota 10)))

(for-each print solved)
