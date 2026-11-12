#!/usr/bin/env ol

; create OpenGL window
(import (lib gl))
(gl:set-window-title "2. Mouse")

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glLineWidth 2.0)
   (glColor3f #xFF/255 #x9A/255 #x00/255)

   (define x      (/ (car mouse) (gl:get-window-width))  )
   (define y (- 1 (/ (cdr mouse) (gl:get-window-height))))
   (glBegin GL_LINES)
      (glVertex2f (- x 0.02) y)
      (glVertex2f (+ x 0.02) y)
      (glVertex2f x (- y 0.02))
      (glVertex2f x (+ y 0.02))
   (glEnd)

))
