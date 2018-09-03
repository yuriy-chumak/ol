#!/usr/bin/ol
;;;; Алгоритм ориентирования и поиска пути в сложном лабиринте
(import
   (owl ff) (otus random!)
   (lib gl))

(define (nth list n)
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))

; ===========================================================================
(define scheme '(
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 1 1 1 2 2 1 1 1 1 1)
   (1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 1 0 0 0 0 2 2 0 0 0 0 1)
   (1 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 1 1 0 0 0 2 2 0 1 0 0 1)
   (1 0 0 0 1 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 1  0 0 0 0 2 2 0 0 2 2 0 0 0 1)
   (1 0 1 1 1 1 1 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 3 0 2 2 0 2 2 0 0 0 1)
   (1 0 1 0 0 0 1 0 0 1 0 0 0 0 0 1 0 0 0 0 1 1 1 0 1 1  0 0 0 0 0 0 2 2 2 2 0 1 0 1)
   (1 0 1 0 0 0 1 0 0 1 1 1 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 3 0 0 2 2 0 0 0 0 1)
   (1 0 1 0 0 0 1 0 0 0 0 1 1 0 1 1 1 1 0 1 1 0 0 0 0 1  0 3 0 0 0 0 0 2 2 2 0 0 0 1)
   (1 0 1 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 0 0 0 2 2 2 0 0 0 1)
   (1 0 1 0 0 0 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0 3 0 0 0 0 2 2 0 0 0 0 1)
   (1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 0 0 0 2 2 0 0 0 0 1)
   (1 0 0 0 0 0 0 0 0 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1  0 0 0 3 0 0 0 2 2 0 0 0 0 1)
   (1 0 1 0 0 0 0 0 0 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 1 0 0 0 0 2 2 2 0 0 0 0 1)
   (1 0 1 0 0 0 0 0 0 1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 2 0 0 0 0 0 1)
   (1 0 1 1 1 1 1 1 1 1 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 0 0 0 0 3 0 1)
   (1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 2 2 0 3 0 0 0 0 1)
   (1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0 0 0 0 1  0 0 0 0 2 2 0 0 0 0 0 0 0 1)
   (1 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 1  0 0 0 0 0 0 0 0 0 0 3 0 0 1)
   (1 0 0 0 0 0 1 0 0 0 0 0 1 0 0 1 0 0 0 0 1 0 0 0 0 1  0 0 0 0 2 2 0 0 0 0 0 0 0 1)
   (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  1 1 1 1 2 2 1 1 1 1 1 1 1 1)))

(define (at x y)
   ;(if (and (>= x 0) (>= y 0))
   (nth (nth scheme y) x))

(define (ne? a b) (not (eq? a b)))

(define (at2 x y scheme)
   (nth (nth scheme y) x))

(define (safe-nth list n)
   (if (and list (not (null? list)))
   (if (= n 0) (car list)
               (safe-nth (cdr list) (- n 1)))))

(define (safe-at x y)
   (if (and (>= x 0) (>= y 0))
      (safe-nth (safe-nth scheme y) x)))


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

; загрузка модуля с ai
,load "ai.lisp"

(define me (new-creature 1 1))
;(mail me (tuple 'update-fov scheme))

(gl:set-window-title "Pathfinder sample")
(import (OpenGL version-1-1))
(import (OpenGL EXT bgra))


(define (quad x y)
   (glVertex2f x y)
   (glVertex2f x (+ y 1))
   (glVertex2f (+ x 1) (+ y 1))
   (glVertex2f (+ x 1) y))

(define (quadT x y u v)
   (glTexCoord2f    u         v)
   (glVertex2f x y)
   (glTexCoord2f    u      (+ v 1/8))
   (glVertex2f x (+ y 1))
   (glTexCoord2f (+ u 1/8) (+ v 1/8))
   (glVertex2f (+ x 1) (+ y 1))
   (glTexCoord2f (+ u 1/8)    v)
   (glVertex2f (+ x 1) y))



(define (draw-map-cell x y cell alpha)
   (cond
   ((eq? cell 2) ; вода
      (glColor3f 0 (* alpha 0.4) alpha)
      (quadT x y 6/8 6/8))
   ((eq? cell 3) ; кусты
      (glColor3f 0 alpha (* alpha 0.4))
      (quadT x y 5/8 5/8))
   ((eq? cell 1)
      (glColor3f alpha alpha alpha)
      (cond
      ; монолитная стена
      ((and (eq? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 3/8))

      ; прямые стены:
      ; сверху вниз
      ((and (ne? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 1/8))

      ; слева направо
      ((and (eq? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 1/8 3/8))

      ; углы:
      ; правый-верхний угол
      ((and (eq? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 4/8 2/8))
      ; правый-нижний угол
      ((and (eq? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 4/8 4/8))
      ; левый-верхний угол
      ((and (ne? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 2/8 2/8))
      ; левый-нижний угол
      ((and (ne? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 2/8 4/8))


      ; ответвления:
      ; влево
      ((and (eq? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 2/8 3/8))
      ; вниз
      ((and (eq? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 4/8))
      ; вверх
      ((and (eq? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 2/8))
      ; вправо
      ((and (ne? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 4/8 3/8))

      ; тупики:
      ; правый тупик
      ((and (eq? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 6/8 3/8))
      ; левый тупик
      ((and (ne? (safe-at (- x 1) y) 1)
            (eq? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 0/8 3/8))
      ; нижний тупик
      ((and (ne? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (eq? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 6/8))
      ; верхний тупик
      ((and (ne? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (eq? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 0/8))

      ; островок
      ((and (ne? (safe-at (- x 1) y) 1)
            (ne? (safe-at (+ x 1) y) 1)
            (ne? (safe-at x (- y 1)) 1)
            (ne? (safe-at x (+ y 1)) 1))
         (quadT x y 3/8 3/8))))))

; окно - рисовалка
; ---------------------------------------------------------------------------

; init
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity) ; тут надо зеркально отразить карту сверху вниз
   (glOrtho -1 (+ WIDTH 1) (+ HEIGHT 3) -3  -1 1)
   (glMatrixMode GL_MODELVIEW)

   (glBindTexture GL_TEXTURE_2D 0)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
      16 16
      0 GL_BGR GL_UNSIGNED_BYTE (file->vector "ground.rgb"))
   (glDisable GL_TEXTURE_2D)

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D 1)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
   (glTexImage2D GL_TEXTURE_2D 0 GL_RGB8
      128 128
      0 GL_BGR GL_UNSIGNED_BYTE (file->vector "tileset.rgb"))

   ;(glEnable GL_BLEND)
(gl:set-userdata #empty)

; draw
(gl:set-renderer (lambda (userdata)
(let*((x (get userdata 'x 14))
      (y (get userdata 'y 1))
      (old-time (get userdata 'old-time 0)))

   (glClearColor 0.2 0.2 0.2 1.)
   (glClear GL_COLOR_BUFFER_BIT)

   ; нарисуем карту как она есть
   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D 0)

   ; сначала пол
   (glBegin GL_QUADS)
      (glColor3f 0.4 0.4 0.4)
      (glTexCoord2f 0 0)
      (glVertex2f   0 0)
      (glTexCoord2f 0 HEIGHT)
      (glVertex2f   0 HEIGHT)
      (glTexCoord2f WIDTH HEIGHT)
      (glVertex2f   WIDTH HEIGHT)
      (glTexCoord2f WIDTH 0)
      (glVertex2f   WIDTH 0)
   (glEnd)

   ; а теперь стены для пользователя
   (glBindTexture GL_TEXTURE_2D 1)
   (glBegin GL_QUADS)
      (let by-y ((y 0) (line scheme))
      (if (not (null? line)) (begin
         (let by-x ((x 0) (cell (car line)))
         (if (not (null? cell)) (begin
            (draw-map-cell x y (car cell) 0.2)
            (by-x (+ x 1) (cdr cell)))))
         (by-y (+ y 1) (cdr line)))))
   (glEnd)
   (glDisable GL_TEXTURE_2D)

   ; будем что-то делать только раз в секунду
   (let*((new-time _ (clock))
         (step (if #t ;(> new-time old-time)
                  (begin
                    (mail me (tuple 'update-fov scheme))
                    (interact me (tuple 'A* x y)))
                  (tuple 0 0 #empty #empty))))

      ; попросим монстра осмотреться


      ; нарисуем карту, которую "помнит" создание
      (if #t
      (let ((map (interact me (tuple 'get-fov))))
         (glEnable GL_TEXTURE_2D)
         (glBegin GL_QUADS)
         (for-each (lambda (i)
            (for-each (lambda (j)
               (if (> (at2 i j map) 0)
                  (let ((age (at2 i j map))) ; как давно оно клетку "видело"
                     (let ((color (/ 1.0 (/ (- 100 age) 7))))
                     ;(glColor3f color color color)
                     (draw-map-cell i j (at i j) 1)))))
               (lrange 0 1 HEIGHT)))
            (lrange 0 1 WIDTH))
         (glEnd))
      )

      ; один из списков
;      (print (ref step 3))
      (if #t
      ;(if (not (and (eq? (ref step 1) 0) (eq? (ref step 2) 0)))
      (let ((map (ref step 3)))
         ; draw 'open list
         (glDisable GL_TEXTURE_2D)
         (glBegin GL_LINES)
         (ff-fold (lambda (n v xyp)
                     (let ((from (ref xyp 1))
                           (to   (ref xyp 2)))
                        (if to
                           (begin
                              (glColor3f 0 1 0)
                              (glVertex2f (+ (car from) 0.5) (+ (cdr from) 0.5))
                              (glColor3f 1 0 0)
                              (glVertex2f (+ (car to) 0.5) (+ (cdr to) 0.5))))))
            #f map)
         (glEnd)
      ))


      ; нарисуем, где сейчас наше создание находится:
      (glEnable GL_TEXTURE_2D)
      (glColor3f 1 1 1)
      (glBegin GL_QUADS)
      (let ((xy (interact me (tuple 'get-location))))
         (if (and (= (car xy) x) (= (cdr xy) y))
            (quadT x y 7/8 1/8)
            (begin
               (quadT (car xy) (cdr xy) 5/8 1/8) ; где сейчас
               (quadT x y 6/8 1/8)))) ; куда собирается пойти
      (glEnd)


      ; все, теперь можем двигаться туда,  куда хотели:
      (mail me (tuple 'move (ref step 1) (ref step 2)))

   ; вернем модифицированные параметры
      (list (put
         (if (and (eq? (ref step 1) 0) (eq? (ref step 2) 0) (> new-time old-time))
            (let do ((x (rand! WIDTH))
                     (y (rand! HEIGHT)))
               (if (eq? (at2 x y scheme) 0)
                  (put (put userdata 'x x) 'y y)
                  (do (rand! WIDTH) (rand! HEIGHT))))
            userdata)
            ; send new
         'old-time new-time))))))

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
;         (lrange 0 1 HEIGHT)))
;      (lrange 0 1 WIDTH)))
;   (glEnd)
;   (glDisable GL_TEXTURE_2D)

;   (glColor3f 0 0 0)
;   (glBegin GL_LINES)
;   (for-each (lambda (i)
;       (glVertex2f i 0)
;       (glVertex2f i HEIGHT))
;      (lrange 0 1 WIDTH))
;   (for-each (lambda (i)
;       (glVertex2f 0 i)
;       (glVertex2f WIDTH i))
;      (lrange 0 1 HEIGHT))
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

