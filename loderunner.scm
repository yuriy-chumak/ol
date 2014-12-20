;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

(define WIDTH 16)
(define HEIGHT 16)

(define (HALF a) (+ 1 (floor (/ a 2))))
(define dx (/ 1.6 WIDTH))
(define dy (/ 1.6 HEIGHT))

(define HW (HALF WIDTH))
(define HH (HALF HEIGHT))


; пара служебных функций
(define (nth list n) ; начиная с 1
   (if (= n 1) (car list)
               (nth (cdr list) (- n 1))))

(define scheme '(
  (9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9)
  (9 0 0 0 0 0 0 0 0 0 0 0 0 0 0 9)
  (9 0 0 0 0 7 0 0 0 0 0 0 0 0 0 9)
  (9 1 1 1 1 1 1 1 2 1 1 1 1 1 1 9)
  (9 0 0 0 0 0 0 0 2 4 4 4 4 4 4 9)
  (9 0 0 0 0 0 0 0 2 0 0 0 1 1 2 9)
  (9 0 0 0 0 0 0 0 2 0 0 0 1 1 2 9)
  (9 1 1 2 1 1 1 1 1 0 0 0 1 1 1 9)
  (9 0 0 2 0 0 0 0 0 0 0 0 0 0 0 9)
  (9 1 1 1 1 1 1 1 1 1 2 1 1 1 1 9)
  (9 0 0 0 0 0 0 0 0 0 2 0 0 0 0 9)
  (9 0 0 0 0 0 0 0 7 0 2 4 4 4 4 9)
  (9 0 0 0 0 2 1 1 1 1 1 1 0 0 0 9)
  (9 0 0 0 0 2 0 0 0 0 0 0 0 0 0 9)
  (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
  (1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)))

(define window (create-window "OL OpenGL Sample 1" 720 720))
(define man (load-bmp "x.bmp"))        ; ходит
(define man2 (load-bmp "y.bmp"))       ; лазает
(define man3 (load-bmp "z.bmp"))       ; падает
(define brick (load-bmp "brick.bmp"))  ; 1
(define gold (load-bmp "gold.bmp"))    ; 7
(define stair (load-bmp "stair.bmp"))  ; 2
(define bar (load-bmp "crossbar.bmp")) ; 4

(glEnable GL_BLEND)
(glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA)


(define (at x y)
   (nth (nth scheme (- (+ HEIGHT 1) y)) x))

(define (quad i j)
   (let ((a (* (- i HW) dx))
         (b (* (- j HH) dy)))
      (glVertex2f a b)
      (glVertex2f a (+ b dy))
      (glVertex2f (+ a dx) (+ b dy))
      (glVertex2f (+ a dx) b)))

(define (quadT i j)
   (let ((a (* (- i HW) dx))
         (b (* (- j HH) dy)))
      (glTexCoord2f 0 0)
      (glVertex2f a b)
      (glTexCoord2f 0 1)
      (glVertex2f a (+ b dy))
      (glTexCoord2f 1 1)
      (glVertex2f (+ a dx) (+ b dy))
      (glTexCoord2f 1 0)
      (glVertex2f (+ a dx) b)))

(define (sprite i j tex)
   (let ((a (* (- i HW) dx))
         (b (* (- j HH) dy)))
      (glBindTexture GL_TEXTURE_2D tex)
      (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f a b)
      (glTexCoord2f 0 1)
      (glVertex2f a (+ b dy))
      (glTexCoord2f 1 1)
      (glVertex2f (+ a dx) (+ b dy))
      (glTexCoord2f 1 0)
      (glVertex2f (+ a dx) b)
      (glEnd)))

; окно - рисовалка
(define (my-renderer ms  userdata)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 3)))
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

   (for-each (lambda (i)
      (for-each (lambda (j)
         (let ((texture (case (at i j)
                           (1 brick)
                           (2 stair)
                           (4 bar)
                           (7 gold)
                           (else #false))))
            (if texture
               (sprite i j (get texture 'id 0)))))
         (iota 1 1 (+ HEIGHT 1))))
      (iota 1 1 (+ WIDTH 1)))

   ; а теперь - кубик игрока
   (sprite x y (get
      (if (= (at x y) 4)
         man2
         (if (= (at x (- y 1)) 0)
            man3
            man)) 'id 0))
;   (glBindTexture GL_TEXTURE_2D (get man 'id 0))
;   (glBegin GL_QUADS)
;      (quadT x y)
;   (glEnd)

   ; вернем модифицированные параметры
   (list userdata)))


; запуск opengl
(mail 'opengl (tuple 'set-userdata #empty))
(mail 'opengl (tuple 'register-renderer my-renderer
   '())) ; renderer, state of renderer
   
(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 3)))

;   (print "x, y: " x ", " y)
;   (print "(at x y): " (at x y))
;   (print "(at x+1 y): " (at (+ x 1) y))

   (if (and
          (or
            (= (at x (- y 1)) 0)
            (= (at x (- y 1)) 4))
          (not (= (at x y) 4)))
      (put userdata 'y (- y 1))
   ;else
   (case key
      ; праворуч
      (39
         (if (or 
               (= (at (+ x 1) y) 0)
               (= (at (+ x 1) y) 2)
               (= (at (+ x 1) y) 4)
               (= (at (+ x 1) y) 7))
            (put userdata 'x (+ x 1)) userdata))
      ; налево
      (37
         (if (or 
               (= (at (- x 1) y) 0)
               (= (at (- x 1) y) 2)
               (= (at (- x 1) y) 4)
               (= (at (- x 1) y) 7))
            (put userdata 'x (- x 1)) userdata))
      ; вверх
      (38
         (if (= (at x y) 2)
            (put userdata 'y (+ y 1)) userdata))
      ; вниз
      (40
         (if (or 
               (= (at x (- y 1)) 2)
               (and (= (at x y) 4)
                    (= (at x (- y 1)) 0)))
            (put userdata 'y (- y 1)) userdata))
      (else
         userdata)))
))))

; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(destroy-window window)