;;;; Алгоритм ориентирования и поиска пути в сложном лабиринте
(define *include-dirs* (append *include-dirs* '("../..")))
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))
,load "graphics.lisp"

(define (nth list n)
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))

; ===========================================================================
(define scheme '(
 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1)
 (1 0 1 0 0 0 0 0 0 0 1 1 1 0 0 1)
 (1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 1)
 (1 0 1 1 1 1 1 1 1 0 1 0 1 0 0 1)
 (1 0 1 0 0 0 0 0 1 0 1 0 1 0 0 1)
 (1 0 0 0 0 0 0 0 1 0 0 0 1 0 0 1)
 (1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 1)
 (1 0 1 1 1 1 1 1 1 1 1 0 1 0 0 1)
 (1 0 1 0 0 0 0 0 0 0 1 0 1 0 0 1)
 (1 0 1 0 0 0 0 0 0 0 0 0 1 0 0 1)
 (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))
(define (at x y)
   (nth (nth scheme y) x))

,load "bresenham1.lisp"

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
(define (is-visible x1 y1 x2 y2)
   (if (or
          (< x1 0)
          (< x2 0)
          (< y1 0)
          (< y2 0)
          (> x1 WIDTH-1)
          (> x2 WIDTH-1)
          (> y1 HEIGHT-1)
          (> y2 HEIGHT-1))
       #f
       (is-point-can-see-point x1 y1 x2 y2)))


;(define WINDOW-WIDTH 640)
;(define WINDOW-HEIGHT (/ (* WINDOW-WIDTH HEIGHT) WIDTH))
(define window (create-window "Pathfinder sample" 640 (/ (* 640 HEIGHT) WIDTH)))
(define floor-texture (load-bmp "stone-floor-texture.bmp"))
(define roof1-texture (load-bmp "roof1.bmp"))
(define water-texture (load-bmp "water-floor-texture.bmp"))
(define grass-texture (load-bmp "grass-floor-texture.bmp"))

(define RECT "0123012301230123")
(GetClientRect window RECT)
(define WINDOW-WIDTH (get-int16 RECT 8))
(define WINDOW-HEIGHT (get-int16 RECT 12))

(print "WINDOW-WIDTH: " WINDOW-WIDTH)
(print "WINDOW-HEIGHT: " WINDOW-HEIGHT)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity) ; вот тут мы зеркально отражаем карту сверху вниз
(glOrtho 0 WIDTH HEIGHT 0  -1 1)
(glMatrixMode GL_MODELVIEW)

(define (to-map-from-screen xy)
(let ((RECT "0123012301230123"))
   (GetClientRect window RECT)
   (let ((window-width (get-int16 RECT 8))
         (window-height (get-int16 RECT 12)))
      (cons (/ (* (car xy) WIDTH) window-width)
            (/ (* (cdr xy) HEIGHT) window-height)))))

(define (get-mouse-pos)
(let ((POINT "01234567"))
   (GetCursorPos POINT)
   (ScreenToClient window POINT)
   (let ((x (get-int16 POINT 0))
         (y (get-int16 POINT 4)))
      (to-map-from-screen (cons x y)))))

;         (y (get-int16 POINT 4)))
;      (print x y)
;      ;(/ (- x (* -WIDTH 0.1)) WIDTH)
;      )))

; окно - рисовалка
; ---------------------------------------------------------------------------
(mail 'opengl (tuple 'register-renderer (lambda (ms userdata)
(let ((x (get userdata 'x 2))
      (y (get userdata 'y 3)))
         
   (glClearColor 0.2 0.2 0.2 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glDisable GL_TEXTURE_2D)

   ; поле
   (glColor3f 1.0 1.0 1.0)
   (glEnable GL_TEXTURE_2D)
   ; стены (как воду)
   (BindTexture water-texture)
   (glBegin GL_QUADS)
   (for-each (lambda (i)
      (for-each (lambda (j)
         (if (> (at i j) 0)
            (quadT i j)))
         (iota 0 1 HEIGHT)))
      (iota 0 1 WIDTH))
   (glEnd)
   ; пол (как травку)
   (BindTexture grass-texture)
   (glBegin GL_QUADS)
   (let* ((my (get userdata 'mouse (cons 0 0)))
          (x (car my))
          (y (cdr my)))
   (for-each (lambda (i)
      (for-each (lambda (j)
         (if (= (at i j) 0)
            (let ((c (+
                        (if (is-visible i j             x y) 0.25 0)
                        (if (is-visible (+ i 1) j       x y) 0.25 0)
                        (if (is-visible (+ i 1) (+ j 1) x y) 0.25 0)
                        (if (is-visible i (+ j 1)       x y) 0.25 0))))
               (glColor3f c c c)
               (quadT i j))))
         (iota 0 1 HEIGHT)))
      (iota 0 1 WIDTH)))
   (glEnd)
   (glDisable GL_TEXTURE_2D)

   (glColor3f 0 0 0)
   (glBegin GL_LINES)
   (for-each (lambda (i)
       (glVertex2f i 0)
       (glVertex2f i HEIGHT))
      (iota 0 1 WIDTH))
   (for-each (lambda (i)
       (glVertex2f 0 i)
       (glVertex2f WIDTH i))
      (iota 0 1 HEIGHT))
   (glEnd)

   
   
   ; нарисуем линию
   #|
   (let ((mx (get userdata 'mouse #f)))
      (if mx (begin
         (glColor3f 0.2 0.2 0.2)
         (let ((xy (get-mouse-pos)))
            (if xy (begin
               (glPointSize 3.0)
               (glColor3f 1 0 0)
               (glBegin GL_POINTS)
               ;(if (is-point-can-see-point (car mx) (cdr mx)  (car xy) (cdr xy))
               (if (is-point-can-see-point (floor (car mx)) (floor (cdr mx))  (floor (car xy)) (floor (cdr xy)))
                  (glColor3f 0 1 0)
                  (glColor3f 0.2 0.2 0.2))
               (glEnd)
            
               ;(glLineWidth 3.0)
               (glBegin GL_LINES)
                (glVertex2f (floor (car mx)) (floor (cdr mx)))
                ;(glVertex2f (car mx) (cdr mx))
                ;(glVertex2f (car xy) (cdr xy))
                (glVertex2f (floor (car xy)) (floor (cdr xy)))
               (glEnd)))))))#||#
   
   ; осмотр себя любимого на N клеток вокруг
   ;#|
   (let ((ms (get userdata 'mouse #f)))
   (if ms (begin
      (glPointSize 4.0)
      (glColor3f 0 0 1)
      (glBegin GL_POINTS)
      (glVertex2f (car ms) (cdr ms))
      (glEnd)
   )))
   (let ((me (get-mouse-pos))
         (N 2))
   (glColor3f 0 1 0)
   (glBegin GL_LINES)
   (let lookout ((n 0)  (x (+ (floor (car me)) 0.5)) (y (+ (floor (cdr me)) 0.5)))
      (if (= n N)
         #t
      (begin
         (let left-to-right ((x (- (floor x) n)) (y (- (floor y) n))  (i (+ n n)))
            (if (is-visible (car me) (cdr me) x y) (begin
               (glVertex2f x y)
               (glVertex2f (car me) (cdr me))))
            (if (> i 0)
               (left-to-right (+ x 1) y (- i 1))))
         (let top-to-bottom ((x (+ (floor x) n 1)) (y (- (floor y) n))  (i (+ n n)))
            (if (is-visible (car me) (cdr me) x y) (begin
               (glVertex2f x y)
               (glVertex2f (car me) (cdr me))))
            (if (> i 0)
               (top-to-bottom x (+ y 1) (- i 1))))
         (let right-to-left ((x (+ (floor x) n 1)) (y (+ (floor y) n 1))  (i (+ n n)))
            (if (is-visible (car me) (cdr me) x y) (begin
               (glVertex2f x y)
               (glVertex2f (car me) (cdr me))))
            (if (> i 0)
               (right-to-left (- x 1) y (- i 1))))
         (let bottom-to-top ((x (- (floor x) n)) (y (+ (floor y) n 1))  (i (+ n n)))
            (if (is-visible (car me) (cdr me) x y) (begin
               (glVertex2f x y)
               (glVertex2f (car me) (cdr me))))
            (if (> i 0)
               (bottom-to-top x (- y 1) (- i 1))))
         (lookout (+ n 1) x y)))))
   (glEnd)
   #||#

   ; вернем модифицированные параметры
   userdata))))

(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
(call/cc (lambda (return)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 3)))

   ;else
   (case key
      ; вверх
      (38
         (put userdata 'y (+ y 1)) userdata)
      ; вниз
      (40
         (put userdata 'y (- y 1)) userdata)
      (else
         userdata))
))))))

(mail 'opengl (tuple 'set-mouse (lambda (userdata  lbutton rbutton x y)
(call/cc (lambda (return)
   (if lbutton (begin
      (print "left: " x)
      (return (put userdata 'mouse (to-map-from-screen (cons x y))))))
      
   userdata)))))
   
   
; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(destroy-window window)