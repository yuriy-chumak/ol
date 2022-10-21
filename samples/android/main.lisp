#!/usr/bin/env ol
(import (lib gl)) ; TODO: уменьшить количество подгружаемого кода

(import (lib gl-2)
   (scheme inexact))
(import (lib GLU))

(gl:set-window-size 640 720) ; размер окошка окулюса, где-то так

; OpenGL Scene Settings
(define RL_CULL_DISTANCE_NEAR 0.01)
(define RL_CULL_DISTANCE_FAR 1000.0)
(define DEG2RAD (/ #i3.14159265358979323846 #i180.0))

(define FOVY 80.0)   ; найдено экспериментально
(define ASPECT (/ (/ (gl:get-window-width) 2) (gl:get-window-height)))

(define TEXW 2048)
(define W (/ TEXW 2))
(define TEXH 2048)

; ----------------------------------------------------
; camera
(define position '(0 0  0))
(define target   '(0 0 -1)) ; вперед...
(define up       '(0 1  0))
; perspective projection

(define program (gl:create-program
"  //
uniform mat4 ViewMatrix;
uniform mat4 ProjectionMatrix;
uniform mat4 ModelMatrix;
void main()
{
   gl_Position = ProjectionMatrix * ViewMatrix * ModelMatrix * gl_Vertex;
   gl_FrontColor = gl_Color;
}"
"  //
void main()
{
	gl_FragColor = gl_Color;
}" ))

(define (draw-cube)
   ; нарисуем много линий
   ;;(glBegin GL_TRIANGLE_STRIP)
   (glBegin GL_LINE_STRIP)
      (for-each (lambda (x y z  r g b)
            (glColor3f r g b)
            (glVertex3f x y z))
         '(-1  1 -1  1  1  1  1 -1 -1 -1 -1  1 -1  1) ;x
         '( 1  1 -1 -1 -1  1  1  1  1 -1 -1 -1  1  1) ;y
         '( 1  1  1  1 -1  1 -1  1 -1  1 -1 -1 -1 -1) ;z

         '( 1  1  1  1  1  1  1  0  0  0  0  0  0  0)
         '( 1  1  1  0  0  0  0  1  1  1  1  0  0  0)
         '( 1  0  1  0  1  0  1  0  1  0  1  0  1  0) )
   (glEnd))
(import (scheme srfi-27))
(define cubes (map (lambda (i)
      (list
         (* (random-real) i)
         (* (random-real) i)
         (* (random-real) i)))
   (iota 50 1)))

(glEnable GL_DEPTH_TEST)
(glShadeModel GL_SMOOTH)

(define vrapi (load-dynamic-library "libmain.so"))
(define ovr_begin (vrapi fft-void  "begin"))
(define ovr_update (vrapi fft-void "update" fft-int))
(define ovr_flush (vrapi fft-void  "flush"))
(define ovr_end (vrapi fft-void    "end"))

(define started (let*((ss ms (clock))) (cons ss ms)))
(gl:set-renderer (lambda ()
   (define time (let*((ss ms (clock))) (cons ss ms)))
   (define delta (* 32
      (+    (- (car time) (car started))
         (/ (- (cdr time) (cdr started)) 1000))))

      ;; (glMatrixMode GL_PROJECTION)
      ;; (glLoadIdentity)
      ;; (gluPerspective FOVY ASPECT RL_CULL_DISTANCE_NEAR RL_CULL_DISTANCE_FAR)

      ;; (glMatrixMode GL_MODELVIEW)
      ;; (glLoadIdentity)
      ;; (apply gluLookAt (append
      ;;    position target up))

   (define (draw)
      (glEnable GL_SCISSOR_TEST)
      (glDepthMask GL_TRUE)
      (glEnable GL_DEPTH_TEST)
      (glDepthFunc GL_LEQUAL)
      (glEnable GL_CULL_FACE)
      (glCullFace GL_BACK)

      (glClearColor 0 0.2 0.2 1)
      (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

      (define matrix '( #i0 #i0 #i0 #i0
                        #i0 #i0 #i0 #i0
                        #i0 #i0 #i0 #i0
                        #i0 #i0 #i0 #i0  ))
      (glMatrixMode GL_MODELVIEW)
      (for-each (lambda (cube)
            (glLoadIdentity)
            (apply glTranslatef cube)
            (glRotatef delta 1 1 0)

            (glGetFloatv GL_MODELVIEW_MATRIX matrix)
            (glUniformMatrix4fv (glGetUniformLocation program "ModelMatrix") 1 GL_FALSE matrix)

            (draw-cube))
         cubes)

      (glFlush))  ; don't forget!!

   (ovr_begin)

   (glUseProgram program)
   ;; ;; todo: glUseProgram(...)
   (ovr_update 0) (draw) (ovr_flush)
   (ovr_update 1) (draw) (ovr_flush)

   (ovr_end)
))
