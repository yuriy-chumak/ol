; алгоритм проверки виден ли угол из центра некоторого кубика


; todo: оформить ai как fork-server




(define (>= a b)
  (or (> a b) (= a b)))
(define (frac x) (- x (floor x)))
;(define (cfrac x) (- (ceil x) x))

; ---------------------------------------------------------------------------
; assert (start point is free)
(define (horizontal-bresenham x1 y1  x2 y2  dk)
; assert обе точки не внутри стен
; Проверка более горизонтальных чем вертикальных линий
; 1. выровнять стартовую (слева) точку вправо
; 3. проверить первый куб (слева от стартовой) (через индикатор y = -1)
; 4. в цикле проверять правый куб для точки (и левый, если перешли через координатную прямую - по у)
(let* ((x (+ 1 (floor x1)))
       (y (+ y1 (* (- x x1) dk))))
(let loop ((x x) (y y) (oldy -1)  (n (- (ceil x2) x)))
   (if (= n 0)
      #t
   (if (and 
          (not (= y oldy)) ; если пересекли линию стен
          (> (at (- x 1) (floor y)) 0))
      #f
   (if (> (at x (floor y)) 0)
      #f
   (loop (+ x 1) (+ y dk) y (- n 1))))))))
      

(define (vertical-bresenham x1 y1  x2 y2  dk)
(let* ((y (+ 1 (floor y1)))
       (x (+ x1 (* (- y y1) dk))))
(let loop ((x x) (y y) (oldx -1)  (n (- (ceil y2) y)))
   (if (= n 0)
      #t
   (if (and 
          (not (= x oldx)) ; если пересекли линию стен
          (> (at (floor x) (- y 1)) 0))
      #f
   (if (> (at (floor x) y) 0)
      #f
   (loop (+ x dk) (+ y 1) y (- n 1))))))))

(define (is-point-can-see-point x1 y1  x2 y2)
; подразумевается, что начальная и конечная точки НЕ в стене
;(if (and (= (floor x1) (floor x2)) (= (floor y1) (floor y2))) ; если это один и тот же блок
;   #t
(let ((dx (- x2 x1))
      (dy (- y2 y1)))
   (if (> (abs dx) (abs dy)) ; горизонтальный
      (if (> dx 0)
         (horizontal-bresenham x1 y1  x2 y2  (/ dy dx))
         (horizontal-bresenham x2 y2  x1 y1  (/ dy dx)))
      (if (> dy 0)
         (vertical-bresenham x1 y1  x2 y2  (/ dx dy))
         (vertical-bresenham x2 y2  x1 y1  (/ dx dy))))))

(define (is-visible x1 y1 x2 y2)
   (and (> x1 0) (> y1 0) (< x1 WIDTH) (< y1 HEIGHT)
        (> x2 0) (> y2 0) (< x2 WIDTH) (< y2 HEIGHT)
        (is-point-can-see-point x1 y1 x2 y2)))

; -=( get-waypoints )=-----------------------------------------------
; возвращает список вейпоинтов, видимых из данной точки карты на N клеток
(define (is-corner-rt x y) ; левый нижний угол блока?
   (and
      (= (at x y) 0)
      (> (at (- x 1) y) 0)
      (= (at (- x 1) (- y 1)) 0)
      (= (at x (- y 1)) 0)))
(define (is-corner-lt x y) ; левый нижний угол блока?
   (and
      (> (at x y) 0)
      (= (at (- x 1) y) 0)
      (= (at (- x 1) (- y 1)) 0)
      (= (at x (- y 1)) 0)))
(define (is-corner-lb x y) ; левый нижний угол блока?
   (and
      (= (at x y) 0)
      (= (at (- x 1) y) 0)
      (= (at (- x 1) (- y 1)) 0)
      (> (at x (- y 1)) 0)))
(define (is-corner-rb x y) ; левый нижний угол блока?
   (and
      (= (at x y) 0)
      (= (at (- x 1) y) 0)
      (> (at (- x 1) (- y 1)) 0)
      (= (at x (- y 1)) 0)))
      
(define (find-point-in-list list xy)
   (if (null? list)
      #f
   (if (and (= (car (car list)) (car xy))
            (= (cdr (car list)) (cdr xy)))
      #t
   (find-point-in-list (cdr list) xy))))
   
(define (add-waypoint xy list)
   (if (find-point-in-list list xy)
      list
      (cons xy list)))

(define (check-corner x y  points)
   (if (is-corner-rt x y)
      (add-waypoint (cons (+ x 0.5) y)
      (add-waypoint (cons x (- y 0.5)) points))
   (if (is-corner-lt x y)
      (add-waypoint (cons (- x 0.5) y)
      (add-waypoint (cons x (- y 0.5)) points))
   (if (is-corner-lb x y)
      (add-waypoint (cons (- x 0.5) y)
      (add-waypoint (cons x (+ y 0.5)) points))
   (if (is-corner-rb x y)
      (add-waypoint (cons (+ x 0.5) y)
      (add-waypoint (cons x (+ y 0.5)) points))
   points)))))


(define (get-waypoints me N)
; fixme: вейпоинты в списке могут дублироваться

(let lookout ((n 0) (x (floor (car me))) (y (floor (cdr me)))  (points '()))
   (if (= n N)
      points
      (let left-to-right ((x x) (y y) (i (+ n n 1))  (points points))
         (if (> i 0)
            (left-to-right (+ x 1) y (- i 1)  (if (is-visible (car me) (cdr me) x y) (check-corner x y  points) points))
      (let top-to-bottom ((x x) (y y) (i (+ n n 1))  (points points))
         (if (> i 0)
            (top-to-bottom x (+ y 1) (- i 1)  (if (is-visible (car me) (cdr me) x y) (check-corner x y  points) points))
      (let right-to-left ((x x) (y y) (i (+ n n 1))  (points points))
         (if (> i 0)
            (right-to-left (- x 1) y (- i 1)  (if (is-visible (car me) (cdr me) x y) (check-corner x y  points) points))
      (let bottom-to-top ((x x) (y y) (i (+ n n 1))  (points points))
         (if (> i 0)
            (bottom-to-top x (- y 1) (- i 1)  (if (is-visible (car me) (cdr me) x y) (check-corner x y  points) points))
      (lookout (+ n 1) (- x 1) (- y 1) points))))))))))))


;(print
;(horizontal-bresenham 1 1  4.5 3))
;(halt 0)
