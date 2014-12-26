;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

; пара служебных функций
(define (nth list n) ; начиная с 1
   (if (= n 0) (car list)
               (nth (cdr list) (- n 1))))
(define (set-nth! list value n)
   (if (= n 0) (set-car! list value)
               (set-nth! list value n)))

(define WIDTH 8)
(define HEIGHT 8)

(define scheme '(
   (1 1 1 1 1 1 1 1)
   (1 0 1 0 0 0 0 1)
   (1 0 1 0 1 1 1 1)
   (1 0 0 0 0 0 0 1)
   (1 0 1 1 1 1 0 1)
   (1 0 1 0 0 1 0 1)
   (1 0 1 0 0 0 0 1)
   (1 1 1 1 1 1 1 1)))

(define (at x y)
   (nth (nth scheme x) y))
;(define (at x y)
;   (if (> (L x y) 15) 0 1))

(define (quad i j )
   (let ((a (* (- i (/ WIDTH 2)) dx))
         (b (* (- j (/ HEIGHT 2)) dy)))
      (glVertex2f a b)
      (glVertex2f a (+ b dy))
      (glVertex2f (+ a dx) (+ b dy))
      (glVertex2f (+ a dx) b)))

(define (quadT i j)
   (let ((a (* (- i HALF) dx))
         (b (* (- j HALF) dy)))
      (glTexCoord2f 0 0)
      (glVertex2f a b)
      (glTexCoord2f 0 1)
      (glVertex2f a (+ b dy))
      (glTexCoord2f 1 1)
      (glVertex2f (+ a dx) (+ b dy))
      (glTexCoord2f 1 0)
      (glVertex2f (+ a dx) b)))


(define window (create-window "OL OpenGL Sample 1" 720 720))
(define floor-texture (load-bmp "floor.bmp"))
(define brick-texture (load-bmp "brick.bmp"))

; окно - рисовалка
(mail 'opengl (tuple 'register-renderer (lambda (ms  userdata)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 2)))
   (glClearColor 0 0 0 1)
   (glClear GL_COLOR_BUFFER_BIT)
   (glDisable GL_TEXTURE_2D)

   ; коробочка
   (glColor3f 1 1 1)
   (glBegin GL_LINE_LOOP)
      (glVertex2f -0.8 -0.8)
      (glVertex2f -0.8 +0.8)
      (glVertex2f +0.8 +0.8)
      (glVertex2f +0.8 -0.8)
   (glEnd)

   (glEnable GL_TEXTURE_2D)
   (glColor3f 1 1 1)
   (glBindTexture GL_TEXTURE_2D (get brick 'id 0))

   (glBegin GL_QUADS)
   (for-each (lambda (i)
      (for-each (lambda (j)
         (if (= (at i j) 1) (begin
            (quadT i j))))
         (iota 1 1 (+ SIZE 1))))
      (iota 1 1 (+ SIZE 1)))
   (glEnd)

   ; а теперь - кубик игрока
   (glBindTexture GL_TEXTURE_2D (get man 'id 0))
   (glBegin GL_QUADS)
      (quadT x y)
   (glEnd)

   ; вернем модифицированные параметры
   (list userdata)))


; запуск opengl
   
(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 2)))

;   (print "x, y: " x ", " y)
;   (print "(at x y): " (at x y))
;   (print "(at x+1 y): " (at (+ x 1) y))

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
;
;   (print "userdata: " userdata)
;   (print "key: " key)
;
;   userdata

))))

; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(destroy-window window)