;(sys-prim 1033 1/2 2/3 0.00000000000000001)

; OL
(define *USE_GLBEGIN* 1)

;  (define isCompiled (list->byte-vector '(0 0 0 0)))
;  (sys-prim 1033 isCompiled #false #false)
;(import (lib opengl))

(import
   (owl defmac) (owl primop) (owl io)
   (owl pinvoke)
   (lib windows)
   (lib opengl))


(define (my-renderer ss ms x y dx dy)
   (print ms)

   (glColor3f 0 0.3 0)
   (glBegin GL_TRIANGLES)
     (glVertex2f (- x 0.2) (- y 0.2))
     (glVertex2f (+ x 0.2) (- y 0.2))
     (glVertex2f (- x 0.2) (+ y 0.2))
   (glEnd)

;   (print x "-" y ":" dx "-" dy)
   ; вернем модифицированные параметры
   (let ((dx (if (< x -0.6) (- dx)
             (if (> x  0.6) (- dx)
              dx)))
         (dy (if (< y -0.6) (- dy)
             (if (> y  0.6) (- dy)
              dy))))
      (list
         (+ x (/ (* dx ms) 1000))
         (+ y (/ (* dy ms) 1000))
         dx dy)))
;   (list (+ x 0.001) (+ y 0.001)))


; возвратим модифицированные параметры функции
;(define window
(mail 'opengl (tuple 'create "OL OpenGL Sample 1" 1280 720))
(mail 'opengl (tuple 'register-renderer my-renderer '(0 0 0.7 0.3))) ; renderer, state of renderer

;(mail 'opengl (tuple 'get-window-height))

(main-game-loop (lambda ()
   (= (GetAsyncKeyState 27) 0)))

; KillGLWindow
;(wglMakeCurrent 0 0)
;(wglDeleteContext hRC)
;(ReleaseDC window hDC)
;(DestroyWindow window)
