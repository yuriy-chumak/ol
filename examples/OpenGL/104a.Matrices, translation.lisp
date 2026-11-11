#!/usr/bin/env ol
(import (scheme inexact))

; create OpenGL window
(import (lib gl))
(gl:set-window-title "3. Matrices, translation")

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (define delta (sin (/ (mod (clock-ms) 6283) #i1000)))

   (glLoadIdentity)
   (glTranslatef delta 0 0)
   ;(glMultTransposeMatrixf `(1 0 0 ,delta  0 1 0 0  0 0 1 0  0 0 0 1))

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
