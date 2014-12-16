;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

; пара служебных функций
(define (M x) (if (< x 0) x (- x)))
(define (P x) (if (> x 0) x (- x)))

; ============================================================================
; создадим главное окно
(define window (create-window "OL OpenGL Sample 1" 720 720))


; окно - рисовалка
(define (my-renderer ms  userdata  x y dx dy)
   (glClear GL_COLOR_BUFFER_BIT)

   ; коробочка
   (glColor3f 1 1 1)
   (glBegin GL_LINE_LOOP)
      (glVertex2f -0.8 -0.8)
      (glVertex2f -0.8 +0.8)
      (glVertex2f +0.8 +0.8)
      (glVertex2f +0.8 -0.8)
   (glEnd)

   ; кубик
   (glBegin GL_TRIANGLE_STRIP)
   (let ((C 0.6))
     (glColor3f 0 C 0)
     (glVertex2f (- x 0.1) (- y 0.1))
     
     (glColor3f 0 0 C)
     (glVertex2f (+ x 0.1) (- y 0.1))
     
     (glColor3f C 0 0)
     (glVertex2f (- x 0.1) (+ y 0.1))
     
     (glColor3f C C 0)
     (glVertex2f (+ x 0.1) (+ y 0.1)))
     
   (glEnd)

   ; вернем модифицированные параметры
   (let ((dx (if (< x -0.7) (P dx)
             (if (> x  0.7) (M dx)
              dx)))
         (dy (if (< y -0.7) (P dy)
             (if (> y  0.7) (M dy)
              dy))))
      (list userdata
         (+ x (/ (* dx ms) 3000)) ; x
         (+ y (/ (* dy ms) 3000)) ; y
         dx dy)))                 ; dx, dy


; запуск opengl
(interact 'opengl (tuple 'set-main-window window))
(mail 'opengl (tuple 'set-userdata #empty))
(mail 'opengl (tuple 'register-renderer my-renderer
   '(0 0 0.71 0.3))) ; renderer, state of renderer
   
(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
   (print "userdata: " userdata)
   (print "key: " key)
   userdata)))

; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(interact 'opengl (tuple 'set-main-window #false))

(destroy-window window)
