#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "2. Lines")

(import (OpenGL version-1-0))
(import (lib math))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glLineWidth 2.0)
   (glColor3f #xFF/255 #x9A/255 #x00/255)

   (define x (/ (car mouse) (ref gl:window-dimensions 3)))
   (define y (- 1 (/ (cdr mouse) (ref gl:window-dimensions 4))))
   (glBegin GL_LINES)
      (glVertex2f (- x 0.02) y)
      (glVertex2f (+ x 0.02) y)
      (glVertex2f x (- y 0.02))
      (glVertex2f x (+ y 0.02))
   (glEnd)
))

; further reading:
; https://stackoverflow.com/questions/6017176/gllinestipple-deprecated-in-opengl-3-1