; http://www.rosettacode.org/wiki/Maze_generation
(define *path* (cons "tests/rosettacode" *path*))
(import (otus random!))

(define WIDTH 70)
(define HEIGHT 30)

(define maze
   (map (lambda (?)
         (repeat #b01111 WIDTH)) ; 0 - unvisited, 1111 - all walls exists
      (iota HEIGHT)))
(define (at x y)
   (list-ref (list-ref maze y) x))

; let "invalid" cells are visited
(define (unvisited? x y)
   (if (and (< -1 x WIDTH) (< -1 y HEIGHT))
      (zero? (band (at x y) #b10000))))
(define neighbors '((-1 . 0) (0 . -1) (+1 . 0) (0 . +1)))
(define walls     '( #b10111  #b11011  #b11101  #b11110))
(define antiwalls '( #b11101  #b11110  #b10111  #b11011))

(let loop ((x (rand! WIDTH)) (y (rand! HEIGHT)))
   ; mark current cell as "visited"
   (list-set! (list-ref maze y) x (bor (at x y) #b10000))
   ; if the are unvisited neighbors
   (let try ()
      (if (or
            (unvisited? (- x 1) y) ; left
            (unvisited? x (- y 1)) ; top
            (unvisited? (+ x 1) y) ; right
            (unvisited? x (+ y 1))) ; bottom
         (let*((p (rand! 4))
               (neighbor (list-ref neighbors p)))
            (let ((nx (+ x (car neighbor)))
                  (ny (+ y (cdr neighbor))))
            (if (unvisited? nx ny)
               (let ((ncell (at nx ny)))
                  (list-set! (list-ref maze y) x (band (at x y) (list-ref walls p)))
                  (list-set! (list-ref maze ny) nx (band ncell (list-ref antiwalls p)))
                  (loop nx ny)))
            (try))))))

(display "+")
(for-each (lambda (?) (display "--+")) (iota WIDTH))
(print)
(for-each (lambda (l)
            ; left wall (always)
            (display "|")
            ; draw right wall
            (for-each (lambda (x)
                        (display "  ")
                        (display (if (zero? (band x #b10)) " " "|")))
               l)
            (print)
            (display "+")
            ; draw bottom wall
            (for-each (lambda (x)
                        (display (if (zero? (band x #b01)) "  " "--"))
                        (display "+"))
               l)
            (print))
   maze)
(print)
