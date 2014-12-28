; алгоритм проверки виден ли угол из центра некоторого кубика

(define (>= a b)
  (or (> a b) (= a b)))
(define (frac x) (- x (floor x)))
(define (cfrac x) (- (ceil x) x))

; ---------------------------------------------------------------------------
; Вырожденные случаи идеальных лучей
; 1. идеально горизонтальный (слева направо)
(define (horizontal-bresenham= x1 y x2) #f) ; left-to-right
(define (horizontal-bresenham=2 x1 y x2) ; left-to-right
(let ((x1 (floor x1)) (x2 (ceiling x2)))
(let loop ((x (+ x1 1)) (y y))
   (or (> x x2)
      (and
         (= (at (- x 1) (- (floor y) 1)) 0)
         (= (at (- x 1) (- (ceil y) 1)) 0)
         (loop (+ x 1) y))))))
         
; 2. идеально вертикальный (снизу вверх)
(define (vertical-bresenham= x y1 y2) #f)
(define (vertical-bresenham=2 x y1 y2)
(let ((y1 (floor y1)) (y2 (ceiling y2)))
(let loop ((x x) (y (+ y1 1)))
   (or (> y y2)
      (and
         (= (at (- x 0) (- y 1)) 0)
         (= (at (- x 1) (- y 1)) 0)
         (loop x (+ y 1)))))))
         
; 3. идеально диагональный (не важно откуда и куда)
;     ^
; -dx |  dx
;  dy |  dy
;-----+----->
; -dx |  dx
; -dy | -dy
;
(define (diagonal-bresenham= x1 y1  x2 y2  dk)
   #f
)
; ---------------------------------------------------------------------------


; горизонтальный
(define (horizontal-bresenhamx x1 y1 x2 y2)
(print "(horizontal-bresenham " x1 " " y1 " " x2 " " y2)
; 1. надо выровнять обе точки на границу 1 - левую влево, правую вправо
;(if (or (= x1 x2)
;        (= y1 y2)
;        (< x2 x1)
;        (< y2 y1)
;        (> (- y2 y1) (- x2 x1))) #f

(let* ((dx (- x2 x1))
       (dy (- y2 y1))
       (dk (/ dy dx))
       ; сдвинем левую точку налево, проверки блока на видимость будут идти для x+1
       (y1 (- (y1 (* (frac x1) dk))))
       (x1 (floor x1))
       
       )
(print "x1 = " x1 ", y1 = " y1)

#f))


;(call/cc (lambda (return)
;;   (print "dx: " dx)
;;   (print "dy: " dy)
;;   (print "dk: " dk)
;   
;   (let loop ((x (+ x1 1)) (y (+ y1 dk)))
;;      (print "x: " x "(" (floor x) ")"
;;           ", y: " y "(" (floor y) ")")
;;      (print "#: " (at (- (floor x) 1) (floor y)))
;      (if (>= x x2)
;         (return #t))
;         
;      (if (= (at (- (floor x) 1) (floor y)) 1)
;         (return #f))
;      (if (< (frac y) dk)
;         (if (= (at (- (floor x) 1) (- (floor y) 1)) 1)
;            (return #f)))
;      (loop (+ x 1) (+ y dk))))))))


; assert (start point is free)
(define (horizontal-bresenham x1 y1  x2 y2  dk)
; 1. выравнять стартовую точку вправо
(let ((y (+ y1 (* (- 1 (frac x1)) dk)))
      (x (ceil x1)))
(let loop ((x x) (y y) (oldy y1))
   (if (> x x2)
      #t
   (if (> (at x (floor y)) 0)
      #f
   (if (and 
          (not (= y oldy)) ; если пересекли линию стен
          (> (at (- x 1) (floor y)) 0))
      #f
   (loop (+ x 1) (+ y dk) y)))))))
      

(define (vertical-bresenham x1 y1  x2 y2  dk)
; 1. выравнять стартовую точку вправо
(let ((x (+ x1 (* (- 1 (frac y1)) dk)))
      (y (ceil y1)))
(let loop ((x x) (y y) (oldx x1))
   (if (> y y2)
      #t
   (if (> (at (floor x) y) 0)
      #f
   (if (and 
          (not (= x oldx)) ; если пересекли линию стен
          (> (at (floor x) (- y 1)) 0))
      #f
   (loop (+ x dk) (+ y 1) x)))))))


(define (is-corner-visible-from x1 y1  x2 y2)
(if (> (at (floor x2) (floor y2)) 0)
   #f
(if (> (at (floor x1) (floor y1)) 0)
   #f
(if (and (= x1 x2) (= y1 y2))
   #t
(let ((dx (- x2 x1))
      (dy (- y2 y1)))
      
(print "dx: " dx ", dy: " dy)      
;   (if (= (floor dx) 0)
;      (if (= (floor dy) 0) #t
;      (if (< y1 y2)
;         (vertical-bresenham= x1 y1 y2)
;         (vertical-bresenham= x1 y2 y1)))
;   (if (= (floor dy) 0)
;      (if (= (floor dx) 0) #t
;      (if (< x1 x2)
;         (horizontal-bresenham= x1 y1 x2)
;         (horizontal-bresenham= x2 y1 x1)))
;   (if (= (abs dx) (abs dy))
;      (if (< dx 0)
;         (diagonal-bresenham= x2 y2 x1 y1  (/ dy dx))
;         (diagonal-bresenham= x1 y1 x2 y2  (/ dy dx)))
   (if (> (abs dx) (abs dy)) ; горизонтальный
      (if (> dx 0)
         (horizontal-bresenham x1 y1  x2 y2  (/ dy dx))
         (horizontal-bresenham x2 y2  x1 y1  (/ dy dx)))
      (if (> dy 0)
         (vertical-bresenham x1 y1  x2 y2  (/ dx dy))
         (vertical-bresenham x2 y2  x1 y1  (/ dx dy)))))))))

;(print
;(horizontal-bresenham 1 1  4.5 3))
;(halt 0)
