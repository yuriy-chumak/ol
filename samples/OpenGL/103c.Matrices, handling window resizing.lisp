#!/usr/bin/env ol
(import (lib gl) (lib math))
(gl:set-window-title "3. Matrices, handling window resizing")

(import (OpenGL version-1-0))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(gl:set-resize-handler (lambda (width height)
   (print "new dimensions: " width " " height)
   (glViewport 0 0 width height)))

(define started (let*((ss ms (clock))) (cons ss ms)))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (define now (let*((ss ms (clock))) (cons ss ms)))
   (define delta (* 0.5 (sin (/ (+ (* 1000 (- (car now) (car started))) (- (cdr now) (cdr started))) 500))))

   (glLoadIdentity)
   (glTranslatef delta 0 0)

   (glColor3f 0.2 0.5 0.2)
   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd)))
