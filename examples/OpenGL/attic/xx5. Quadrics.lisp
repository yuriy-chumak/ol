#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "5. Quadrics")

(import (OpenGL 1.0) (lib GLU))
; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

(glMatrixMode GL_PROJECTION)
(glLoadIdentity)
(gluPerspective 45 (/ 640 480) 0.1 100)

(glEnable GL_DEPTH_TEST)

(define sphere (gluNewQuadric))
(gluQuadricDrawStyle sphere GLU_LINE)

(import (scheme dynamic-bindings))
(define q (make-parameter [
      1 0.02 3 0.03 sphere]))

; draw
(gl:set-renderer (lambda ()
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (vector-apply (q) (lambda (x dx y dy sphere)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y 5
      0 0 0
      0 1 0)

   (gluSphere sphere 1 16 8)

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (q [(+ x nx) nx (+ y ny) ny sphere]))))
))
