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
(define (my-renderer ms  userdata)
;   (print "userdata: " userdata)
   (let ((x (get userdata 'x 0))
         (y (get userdata 'y 0)))
         
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

   (list userdata)))


; запуск opengl
(mail 'opengl (tuple 'set-userdata #empty))
;(list->ff '((x 0) (y 0)))))
(mail 'opengl (tuple 'register-renderer my-renderer
   '())) ; renderer, state of renderer
   
(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
   (let ((x (get userdata 'x 0))
         (y (get userdata 'y 0)))
;      (print "userdata: " userdata)
;      (print "key: " key)
   
      (case key
         (27
            (mail 'opengl (tuple 'set-main-window #false))
            (destroy-window window)
            (halt 1))
         (38 (put userdata 'y (+ y 0.1)))
         (40 (put userdata 'y (- y 0.1)))
         (37 (put userdata 'x (- x 0.1)))
         (39 (put userdata 'x (+ x 0.1)))
         
         (else userdata))))))

(mail 'opengl (tuple 'set-main-window window))

(main-game-loop (lambda () #t))

;(wait-mail)
; главный цикл работы приложения
;(main-game-loop (lambda ()
;   (= (GetAsyncKeyState 27) 0)))

;(mail 'opengl (tuple 'set-main-window #false))
;
;(destroy-window window)
;(halt 1)
