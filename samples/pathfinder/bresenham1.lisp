; алгоритм проверки виден ли угол из центра некоторого кубика

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

;(print
;(horizontal-bresenham 1 1  4.5 3))
;(halt 0)
