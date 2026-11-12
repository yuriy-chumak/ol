#!/usr/bin/env ol
(import (scheme inexact))

; create OpenGL window
(import (lib gl))
(gl:set-window-title "3. Matrices")

; import OpenGL functions
(import (OpenGL 1.0)
   (lib GLU))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (define delta (/ (mod (clock-ms) 6283) #i1000))
   (define x (* 3 (sin delta)))
   (define z (* 3 (cos delta)))

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ (gl:get-window-width) (gl:get-window-height)) 0.1 100)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x 2 z
      0 0 0
      0 1 0)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd)

))
