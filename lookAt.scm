;;;; Пример использования библиотеки (lib opengl)
(import
   (owl defmac) (owl primop) (owl io) (owl ff)
   (owl pinvoke)
   (lib windows)
   (lib opengl))

(define % (dlopen "glu32.dll" 0))
(define gluPerspective (dlsym % GLvoid "gluPerspective" GLdouble GLdouble GLdouble GLdouble))
(define gluLookAt (dlsym % GLvoid "gluLookAt" GLdouble GLdouble GLdouble  GLdouble GLdouble GLdouble  GLdouble GLdouble GLdouble))

(define (sin x)
(let ((x (/ (rem (* x 10000) 31415) 10000)))
   (+ x
      (- (/ (* x x x) 6))
         (/ (* x x x x x) 120)
      (- (/ (* x x x x x x x) 5040)))))
;         (/ (* x x x x x x x x x) 362880))))

(define (cos x)
(let ((x (/ (rem (* x 10000) 31415) 10000)))
   (+ 1
      (- (/ (* x x) 2))
         (/ (* x x x x) 24)
      (- (/ (* x x x x x x) 720))
         (/ (* x x x x x x x x) 40320))))

; ============================================================================
; создадим главное окно
(define window (create-window "OL OpenGL LookAt Sample" 640 480))

  (glMatrixMode GL_PROJECTION)
  (glLoadIdentity)
  (gluPerspective 70 640/480  1 10)

; окно - рисовалка
(mail 'opengl (tuple 'register-renderer (lambda (ms  userdata)
(let ((i (get userdata 'angle 0)))
   (print i)
   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt (* 5 (sin i)) 0 (* 5 (cos i))  0 0 0  0 1 0)

   (glClear GL_COLOR_BUFFER_BIT)

   ; коробочка
   (glColor3f 1 1 1)
   (glBegin GL_LINE_LOOP)
      (glVertex2f -0.8 -0.8)
      (glVertex2f -0.8 +0.8)
      (glVertex2f +0.8 +0.8)
      (glVertex2f +0.8 -0.8)
   (glEnd)

;   (list userdata))) '()))
   (list
   (put userdata 'angle (+ i (* ms 0.001)))))) '()))

;(mail 'opengl (tuple 'set-keyboard (lambda (userdata  key)
;   (print "userdata: " userdata)
;   (print "key: " key)
;   userdata)))

; главный цикл работы приложения
(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

(destroy-window window)