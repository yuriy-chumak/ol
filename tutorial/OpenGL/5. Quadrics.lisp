#!/usr/bin/ol
(import (lib linux opengl) (owl io))

(gl:run "5. Quadrics" 640 480

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1)

   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 (/ 640 480) 0.1 100)

   (glEnable GL_DEPTH_TEST)
   (let ((sphere (gluNewQuadric)))
      (gluQuadricDrawStyle sphere GLU_LINE)

   (list 1 0.02 3 0.03 sphere)))

; draw
(lambda (x   dx y   dy  sphere)
   (glClear (fx:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt x y 5
      0 0 0
      0 1 0)

   (gluSphere sphere 1 16 8)

   (let ((nx (if (or (> x 2) (< x -2)) (- dx) dx))
         (ny (if (or (> y 4) (< y -4)) (- dy) dy)))
      (list (+ x nx) nx (+ y ny) ny sphere))
))