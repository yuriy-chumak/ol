#!/usr/bin/ol
;;;; Алгоритм ориентирования и поиска пути в сложном лабиринте
(import
   (owl ff) (owl random!)
   (owl pinvoke)
   (lib opengl)
   (OpenGL version-1-1)
)

(define (quad x y)
   (glVertex2f x y)
   (glVertex2f x (+ y 1))
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f (+ x 1) y))


(define (nth list n)
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))

; ===========================================================================
(define scheme '(
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
   (1 0 1 0 0 0 0 0 0 0 0 1 0 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 1 1 1 0 0 1)
   (1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 1)
   (1 0 1 1 1 1 1 1 1 0 1 0 1 1 1 1) ;0 0 1
   (1 0 1 0 0 0 0 0 1 0 1 0 1 0 0 1)
   (1 0 0 0 0 0 0 0 1 0 0 0 1 1 0 1)
   (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1)
   (1 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 1 0 1 0 0 1)
   (1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
(define (at x y)
   (nth (nth scheme y) x))
(define (at2 x y scheme)
   (nth (nth scheme y) x))


; константы
(define WIDTH (length (car scheme)))
(define -WIDTH (- WIDTH))
(define +WIDTH (+ WIDTH))
(define HEIGHT (length scheme))
(define -HEIGHT (- HEIGHT))
(define +HEIGHT (+ HEIGHT))
(print "WIDTH: " WIDTH)
(print "HEIGHT: " HEIGHT)

(define WIDTH-1 (- WIDTH 1))
(define HEIGHT-1 (- HEIGHT 1))

,load "ai.lisp"

(define me (new-creature 1 1))
(mail me (tuple 'update-fov scheme))

(define (quad x y)
   (glVertex2f x y)
   (glVertex2f x (+ y 1))
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f (+ x 1) y))



(define Context (gl:Create "Pathfinder sample"))

; окно - рисовалка
; ---------------------------------------------------------------------------

(gl:run
   Context

; init
(lambda ()

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; вот тут мы зеркально отражаем карту сверху вниз
   (glOrtho -1 (+ WIDTH 1) (+ HEIGHT 1) -1  -1 1)
   (glMatrixMode GL_MODELVIEW)

   (list #empty))

; draw
(lambda (userdata)
(let*((x (get userdata 'x 14))
      (y (get userdata 'y 1))
      (old-time (get userdata 'old-time 0)))

   (glClearColor 0.2 0.2 0.2 1.)
   (glClear GL_COLOR_BUFFER_BIT)

; нарисуем карту как она есть
   (if #t (begin
   (glBegin GL_QUADS)
      (let by-y ((y 0) (line scheme))
      (if (not (null? line)) (begin
         (let by-x ((x 0) (cell (car line)))
         (if (not (null? cell)) (begin
            (case (car cell)
               (0 (glColor3f 0.1 0.1 0.1))
               (1 (glColor3f 0.7 0.7 0.7)))
            (quad x y)
            (by-x (+ x 1) (cdr cell)))))
         (by-y (+ y 1) (cdr line)))))
   (glEnd)
   ))

   ; будем что-то делать только раз в секунду
   (let*((new-time _ (clock))
         (step (if #t;(> new-time old-time)
                  (let ((ne
                  (interact me (tuple 'A* x y))))
                     (mail me (tuple 'update-fov scheme))
                     ne)
                  (cons 0 0))))

      (if (not (and (eq? (car step) 0) (eq? (cdr step) 0)))
         (mail me (tuple 'move (car step) (cdr step))))

      ; попросим монстра осмотреться


      ; нарисуем карту, которую "помнит" создание
      (if #f
      (let ((map (interact me (tuple 'get-fov))))
         (glColor3f 0 1 0)
         (glBegin GL_QUADS)
         (for-each (lambda (i)
            (for-each (lambda (j)
               (if (> (at2 i j map) 0)
                  (let ((age (at2 i j map))) ; как давно оно клетку "видело"
                     (let ((color (/ 1.0 (/ (- 100 age) 7))))
                     (glColor3f color color color)
                     (quad i j)))
                  (begin
                     (glColor3f 0 0 0)
                     (quad i j))
               ))
               (iota 0 1 HEIGHT)))
            (iota 0 1 WIDTH))
         (glEnd))
      )

      ; открытый список
      (if #f
      (let ((map (interact me (tuple 'A* 14 1))))
         ; draw 'open list
         (glBegin GL_POINTS)
         (glColor3f 0 0 1)
         (print "-- open-list -------")
         (ff-fold (lambda (n v xyp)
;                     (glColor3f 0 0 (/ (- 10 (ref xyp 3)) 10))
                     (print "> " v ":" xyp " ^" (/ (- 10 (ref xyp 3)) 10))
                     (let ((xy (ref xyp 1)))
                     (glVertex2f (+ (car xy) 0.5) (+ (cdr xy) 0.5))))
            #f (car map))
         (print "-- closed-list -------")

         (glColor3f 0 1 0)
         (ff-fold (lambda (n v xyp)
;                     (glColor3f 0 (/ (- 10 (ref xyp 3)) 10) 0)
                     (print "> " v ":" xyp)
                     (let ((xy (ref xyp 1)))
                     (glVertex2f (+ (car xy) 0.5) (+ (cdr xy) 0.5))))
            #f (cdr map))
         (print "-- end -------------")
         (glEnd)
      ))


      ; нарисуем, где сейчас наше создание находится:
      (glPointSize 6)
      (glBegin GL_POINTS)
      (let ((xy (interact me (tuple 'get-location))))
         (glPointSize 9.0)
            (if (and (= (car xy) x) (= (cdr xy) y))
               (begin
                  (glColor3f 1 1 0)
                  (glVertex2f (+ x 0.5) (+ y 0.5)))
               (begin
            (glColor3f 1 0 0)
            (glVertex2f (+ (car xy) 0.5) (+ (cdr xy) 0.5))
            (glColor3f 0 1 0) ; и куда собирается пойти
            (glVertex2f (+ x 0.5) (+ y 0.5))))
      (glEnd)
      (glPointSize 0)

   ; вернем модифицированные параметры
      (list (put
         (if (and (eq? (car step) 0) (eq? (cdr step) 0) (> new-time old-time))
            (let do ((x (rand! WIDTH))
                     (y (rand! HEIGHT)))
               (if (eq? (at2 x y scheme) 0)
                  (put (put userdata 'x x) 'y y)
                  (do (rand! WIDTH) (rand! HEIGHT))))
            userdata)
            ; send new
         'old-time new-time)))))))

   ; пол (как травку)
;   (BindTexture grass-texture)
;   (glBegin GL_QUADS)
;   (let* ((my (get userdata 'mouse (cons 0 0)))
;          (x (car my))
;          (y (cdr my)))
;   (for-each (lambda (i)
;      (for-each (lambda (j)
;         (if (= (at i j) 0)
;            (let ((c (+
;                        (if (is-visible i j             x y) 0.25 0)
;                        (if (is-visible (+ i 1) j       x y) 0.25 0)
;                        (if (is-visible (+ i 1) (+ j 1) x y) 0.25 0)
;                        (if (is-visible i (+ j 1)       x y) 0.25 0))))
;               (glColor3f c c c)
;               (quadT i j))))
;         (iota 0 1 HEIGHT)))
;      (iota 0 1 WIDTH)))
;   (glEnd)
;   (glDisable GL_TEXTURE_2D)

;   (glColor3f 0 0 0)
;   (glBegin GL_LINES)
;   (for-each (lambda (i)
;       (glVertex2f i 0)
;       (glVertex2f i HEIGHT))
;      (iota 0 1 WIDTH))
;   (for-each (lambda (i)
;       (glVertex2f 0 i)
;       (glVertex2f WIDTH i))
;      (iota 0 1 HEIGHT))
;   (glEnd)

   
   
   ; нарисуем линию
;   #|
;   (let ((mx (get userdata 'mouse #f)))
;      (if mx (begin
;         (glColor3f 0.2 0.2 0.2)
;         (let ((xy (get-mouse-pos)))
;            (if xy (begin
;               (glPointSize 3.0)
;               (glColor3f 1 0 0)
;               (glBegin GL_POINTS)
;               ;(if (is-point-can-see-point (car mx) (cdr mx)  (car xy) (cdr xy))
;               (if (is-point-can-see-point (floor (car mx)) (floor (cdr mx))  (floor (car xy)) (floor (cdr xy)))
;                  (glColor3f 0 1 0)
;                  (glColor3f 0.2 0.2 0.2))
;               (glEnd)
;            
;               ;(glLineWidth 3.0)
;               (glBegin GL_LINES)
;                (glVertex2f (floor (car mx)) (floor (cdr mx)))
;                ;(glVertex2f (car mx) (cdr mx))
;                ;(glVertex2f (car xy) (cdr xy))
;                (glVertex2f (floor (car xy)) (floor (cdr xy)))
;               (glEnd)))))))
;   #||#

;   (let ((ms (get userdata 'mouse #f)))
;   (if ms (begin
;      (glPointSize 4.0)
;      (glColor3f 0 0 1)
;      (glBegin GL_POINTS)
;      (glVertex2f (car ms) (cdr ms))
;      (glEnd)
;   )))

   ; осмотр себя любимого на N клеток вокруг
;   |#
;   (let ((me (get-mouse-pos))
;         (N 6))
;   (glColor3f 0 1 0)
;   (glBegin GL_LINES)
;   (let lookout ((n 0)  (x (+ (floor (car me)) 0.5)) (y (+ (floor (cdr me)) 0.5)))
;      (if (= n N)
;         #t
;      (begin
;         (let left-to-right ((x (- (floor x) n)) (y (- (floor y) n))  (i (+ n n)))
;            (if (= i 0)
;               #f
;            (begin
;               (if (is-visible (car me) (cdr me) x y) (begin
;                  (glVertex2f x y)
;                  (glVertex2f (car me) (cdr me))))
;               (left-to-right (+ x 1) y (- i 1)))))
;         (let top-to-bottom ((x (+ (floor x) n 1)) (y (- (floor y) n))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y) (begin
;               (glVertex2f x y)
;               (glVertex2f (car me) (cdr me))))
;            (if (> i 0)
;               (top-to-bottom x (+ y 1) (- i 1))))
;         (let right-to-left ((x (+ (floor x) n 1)) (y (+ (floor y) n 1))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y) (begin
;               (glVertex2f x y)
;               (glVertex2f (car me) (cdr me))))
;            (if (> i 0)
;               (right-to-left (- x 1) y (- i 1))))
;         (let bottom-to-top ((x (- (floor x) n)) (y (+ (floor y) n 1))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y) (begin
;               (glVertex2f x y)
;               (glVertex2f (car me) (cdr me))))
;            (if (> i 0)
;               (bottom-to-top x (- y 1) (- i 1))))
;         (lookout (+ n 1) x y)))))
;   (glEnd)
;   #||#
   
   
   ; хорошо, теперь попробуем построить список ключевых точек (вейпоинтов), про которые мы теперь знаем, что они на карте есть
   ;  
   ;  для начала - по тем, что сверху
;   #|
;   (let* ((me (get-mouse-pos))
;          (points (get-waypoints me 7)))
;
;      (glColor3f 0 0 1)
;      (glPointSize 4.0)
;      (glBegin GL_POINTS)
;      
;      (let draw ((p points))
;         (if (null? p)
;            #t
;            (begin
;               (glVertex2f (car (car p)) (cdr (car p)))
;               (draw (cdr p)))))
;      (glEnd))
;   #||#




;         (let top-to-bottom ((x (+ (floor x) n 1)) (y (- (floor y) n))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y)
;               (check-corner x y))
;            (if (> i 0)
;               (top-to-bottom x (+ y 1) (- i 1))))
;         (let right-to-left ((x (+ (floor x) n 1)) (y (+ (floor y) n 1))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y)
;               (check-corner x y))
;            (if (> i 0)
;               (right-to-left (- x 1) y (- i 1))))
;         (let bottom-to-top ((x (- (floor x) n)) (y (+ (floor y) n 1))  (i (+ n n)))
;            (if (is-visible (car me) (cdr me) x y)
;               (check-corner x y))
;            (if (> i 0)
;               (bottom-to-top x (- y 1) (- i 1))))
      

;(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
;(call/cc (lambda (return)
;   ;else
;   (case key
;      (32
;         (mail me (tuple 'update-fov scheme)))
;         
;      (39
;         (mail me (tuple 'move +1 0))
;         (mail me (tuple 'update-fov scheme)))
;      (37
;         (mail me (tuple 'move -1 0))
;         (mail me (tuple 'update-fov scheme)))
;
;      ; вверх
;      (38
;         (mail me (tuple 'move 0 -1))
;         (mail me (tuple 'update-fov scheme)))
;      ; вниз
;      (40
;         (mail me (tuple 'move 0 +1))
;         (mail me (tuple 'update-fov scheme))))
;         
;   userdata)))))
;
;(mail 'opengl (tuple 'set-mouse (lambda (userdata  lbutton rbutton x y)
;(call/cc (lambda (return)
;   (if lbutton (begin
;      (mail me (tuple 'set-location (to-map-from-screen (cons x y))))
;      (mail me (tuple 'update-fov scheme))
;      (return (put userdata 'mouse (to-map-from-screen (cons x y))))))
;   userdata)))))
   
)