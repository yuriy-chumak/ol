(define *include-dirs* (append *include-dirs* '("../..")))
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

(define % (dlopen "glu32.dll" 0))
(define gluPerspective (dlsym % GLvoid "gluPerspective" GLdouble GLdouble GLdouble GLdouble))
(define gluLookAt (dlsym % GLvoid "gluLookAt" GLdouble GLdouble GLdouble  GLdouble GLdouble GLdouble  GLdouble GLdouble GLdouble))

(define (BindTexture texture)
   (glBindTexture GL_TEXTURE_2D (get texture 'id 0)))

(define (sin x)
   (+ x
      (- (/ (* x x x) 6))
         (/ (* x x x x x) 120)
      (- (/ (* x x x x x x x) 5040))))
;        (/ (* x x x x x x x x x) 362880))))

(define (cos x)
   (+ 1
      (- (/ (* x x) 2))
         (/ (* x x x x) 24)
      (- (/ (* x x x x x x) 720))
         (/ (* x x x x x x x x) 40320)))

; пара служебных функций
(define (nth list n) ; начиная с 1
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))
(define (set-nth! list value n)
   (if (= n 0) (set-car! list value)
               (set-nth! list value n)))

(define WIDTH 8)
(define HEIGHT 8)
(define HEIGHT-1 (- HEIGHT 1))

; тут координаты идут сверху-вниз, а не снизу вверх.
; а значит результирующая картинку будет перевернута
; прошу это учесть. а вообще, переделал на прямую
(define scheme '(
   (1 1 1 1 1 1 1 1)
   (1 0 1 0 0 0 0 1)
   (1 0 1 0 1 1 1 1)
   (1 0 0 0 0 0 0 1)
   (1 0 1 1 1 1 0 1)
   (1 0 1 0 0 1 0 1)
   (1 0 1 0 0 0 0 1)
   (1 1 1 1 1 1 1 1)
))

(define (at x y)
   (nth (nth scheme (- HEIGHT-1 y)) x))
;(define (at x y)
;   (if (> (L x y) 15) 0 1))

(define (quad a b)
   (glVertex2f a b)
   (glVertex2f a (+ b 1))
   (glVertex2f (+ a 1) (+ b 1))
   (glVertex2f (+ a 1) b))

(define (quadT a b w h floor)
   (glTexCoord2f 0 0)
   (glVertex3f a b floor)
   (glTexCoord2f 0 h)
   (glVertex3f a (+ b h) floor)
   (glTexCoord2f w h)
   (glVertex3f (+ a w) (+ b h) floor)
   (glTexCoord2f w 0)
   (glVertex3f (+ a w) b floor))

(define (wallT a b w h)
   (glTexCoord2f 0 0)
   (glVertex2f a b)
   (glTexCoord2f 0 h)
   (glVertex2f a (+ b h))
   (glTexCoord2f w h)
   (glVertex2f (+ a w) (+ b h))
   (glTexCoord2f w 0)
   (glVertex2f (+ a w) b))

; Создадим окно и активируем OpenGL контекст:
(define window (create-window "Labyrinth Sample:" 640 480))
(define floor-texture (load-bmp "stone-floor-texture.bmp"))
(define wall1-texture (load-bmp "stone-wall1-texture.bmp"))

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 90.0 640/480 1 100)

; Рисовалки
(define (drawWall a b w h)
   (glTexCoord2f 0 0.0)
   (glVertex3f a b 0.0)
   (glTexCoord2f 1 0.0)
   (glVertex3f (+ a w) (+ b h) 0.0)
   (glTexCoord2f 1 0.2)
   (glVertex3f (+ a w) (+ b h) 0.2)
   (glTexCoord2f 0 0.2)
   (glVertex3f a b 0.2))

(mail 'opengl (tuple 'register-renderer (lambda (ms  userdata)
(let ((x (get userdata 'x 2))
      (y (get userdata 'y 2))
      (i (get userdata 'angle 2)))

   (print i)

   (glClearColor 0 0 0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glDisable GL_TEXTURE_2D)
   (glColor3f 1 1 1)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt (- (/ WIDTH 2)  (* 5 (sin i)))
              (- (/ HEIGHT 2) (* 5 (cos i))) 3
              (/ WIDTH 2) (/ HEIGHT 2) 0
              0 0 1)

   (glEnable GL_TEXTURE_2D)

   ; floor
   (BindTexture floor-texture)
   (glBegin GL_QUADS)
   (quadT 0 0 WIDTH HEIGHT 0)
   (glEnd)

   ; walls
   (BindTexture wall1-texture)
   (glBegin GL_QUADS)
   (for-each (lambda (i)
      (for-each (lambda (j)
         (if (= (at i j) 1)
            (begin
               ;(print i j)
               (drawWall i j 1 0)
               (drawWall i j 0 1)
;               (drawWall i j 1 1)
;               (drawWall i j 0 0)
                
               (quadT i j 1 1 0.2)

            ))) (iota 0 1 HEIGHT))) (iota 0 1 WIDTH))
   (glEnd)

   ; вернем модифицированные параметры
   (put userdata 'angle
      (let ((newi (+ i 0.001)))
         (if (> newi 3.141)
            (- newi 3.141 6.283) ; 2pi = 6,283185307179586476925286766559
            newi)))))))


; запуск opengl
(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 2)))

   (case key
      (39
         (if (= (at (+ x 1) y) 0)
            (put userdata 'x (+ x 1)) userdata))
      (37
         (if (= (at (- x 1) y) 0)
            (put userdata 'x (- x 1)) userdata))
      (38
         (if (= (at x (+ y 1)) 0)
            (put userdata 'y (+ y 1)) userdata))
      (40
         (if (= (at x (- y 1)) 0)
            (put userdata 'y (- y 1)) userdata))
      (else
         userdata))
))))

; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(destroy-window window)