#!/usr/bin/env ol

(import (lib gl))
(gl:set-window-title "3. Matrices")

(import (OpenGL 1.0)
   (lib GLU) (scheme inexact))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(define old (time-ms))

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (define delta (/ (mod (- (time-ms) old) 6283) #i1000))
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
   (glEnd)))
