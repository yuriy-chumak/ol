;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io)
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




(define window (create-window "OL OpenGL Sample 1" 720 720))

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

(define (quad i j)
   (let ((a (* (- i 5) 0.2))
         (b (* (- j 5) 0.2)))
      (glVertex2f a b)
      (glVertex2f a (+ b 0.2))
      (glVertex2f (+ a 0.2) (+ b 0.2))
      (glVertex2f (+ a 0.2) b)))


; окно - рисовалка
(define (my-renderer ms  userdata)
   (let ((x (get userdata 'x 2))
         (y (get userdata 'y 2)))
   (glClear GL_COLOR_BUFFER_BIT)

   ; коробочка
   (glColor3f 1 1 1)
   (glBegin GL_LINE_LOOP)
      (glVertex2f -0.8 -0.8)
      (glVertex2f -0.8 +0.8)
      (glVertex2f +0.8 +0.8)
      (glVertex2f +0.8 -0.8)
   (glEnd)

   (glBegin GL_QUADS)
;   (foreach (iota 1 1 9) (lambda (i)
;      (for-each (iota 1 1 9) (lambda (j)
;         (if (= (at i j) 1)
;            (glColor3f 0 0 0.5)
;            (glColor3f 0 0 0))
;         (quad i j)))))
   (for-each (lambda (i)
      (for-each (lambda (j)
         (if (= (at i j) 1)
            (glColor3f 0 0 0.7)
            (glColor3f 0 0 0))
         (quad i j))
         (iota 1 1 9)))
      (iota 1 1 9))

   ; а теперь - кубик игрока
   (glColor3f 0 0.7 0)
   (quad x y)

   (glEnd)


   ; вернем модифицированные параметры
   (list userdata)))


; запуск opengl
(interact 'opengl (tuple 'set-main-window window))
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

(interact 'opengl (tuple 'set-main-window #false))

(destroy-window window)
