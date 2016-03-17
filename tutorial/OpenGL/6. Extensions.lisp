#!/usr/bin/ol
(import (lib opengl))

(define Context (gl:Create "6. Extensions"))

;(gl:MakeCurrent Context)
(import (OpenGL EXT texture_object))
(import (OpenGL EXT vertex_array))
(import (OpenGL EXT subtexture))
(import (OpenGL ARB shader_object))
;(gl:StopCurrent Context)

(if (not (and
         EXT_texture_object
         EXT_vertex_array
;         ARB_shader_object
         EXT_subtexture))
   (runtime-error "Not all extensions supported" 0))

(gl:run

   Context
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
   (glClear (vm:or GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

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