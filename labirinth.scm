;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

; пара служебных функций
(define (M x) (if (< x 0) x (- x)))
(define (P x) (if (> x 0) x (- x)))

(define (nth list n) ; начиная с 1
   (if (= n 1) (car list)
               (nth (cdr list) (- n 1))))

(define (foreach a b) (for-each b a))

(define (sin x)
   (+ x
      (- (/ (* x x x) 6))
      (/ (* x x x x x) 120)
      (- (/ (* x x x x x x x) 5040))))

(define (cos x)
   (+ 1
      (- (/ (* x x) 2))
         (/ (* x x x x) 24)
      (- (/ (* x x x x x x) 720))))

(define (L x y) (rem (floor (* 10000 (abs (cos (+ (cos x) (* y y)))))) 100))

(define SIZE 20)
(define HALF (+ 1 (floor (/ SIZE 2))))
(define dx (/ 1.6 SIZE))
(define dy (/ 1.6 SIZE))

(define (LL x y)
   (if (or (= x 1) (= x SIZE) (= y 1) (= y SIZE))
      1
      (if (> (L x y) 20) 0 1)))


(define (line y n)
   (map (lambda (i) (LL i y)) (iota 1 1 (+ n 1))))

(define (lines n)
   (map (lambda (j)
      (map (lambda (i)
         (LL i j))
       (iota 1 1 (+ n 1))))
    (iota 1 1 (+ n 1))))


;(let loop ((l '()) (i n))
;   (if (= i 0)
;      l
;      (loop (cons (line i n) l) (- i 1)))))

(define scheme (lines SIZE))
(print scheme)
;'(
;   (1 1 1 1 1 1 1 1)
;   (1 0 1 0 0 0 0 1)
;   (1 0 1 0 1 1 1 1)
;   (1 0 0 0 0 0 0 1)
;   (1 0 1 1 1 1 0 1)
;   (1 0 1 0 0 1 0 1)
;   (1 0 1 0 0 0 0 1)
;   (1 1 1 1 1 1 1 1)))

(define (at x y)
   (nth (nth scheme x) y))
;(define (at x y)
;   (if (> (L x y) 15) 0 1))

(define (quad i j)
   (let ((a (* (- i HALF) dx))
         (b (* (- j HALF) dy)))
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
(define man (load-bmp "x.bmp"))
(define brick (load-bmp "brick.bmp"))

; окно - рисовалка
(define (my-renderer ms  userdata)
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
(mail 'opengl (tuple 'set-userdata #empty))
(mail 'opengl (tuple 'register-renderer my-renderer
   '())) ; renderer, state of renderer
   
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