#!/usr/bin/env ol
(import (scheme inexact))

; create OpenGL window
(import (lib gl))
(gl:set-window-title "2. Extensions usage (EXT polygon_offset)")

; import OpenGL functions
(import (OpenGL 1.0))
(import (OpenGL EXT point_parameters))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

(define attenuation [#i0.0 #i0.0 #i0.1])
(glPointParameterfv GL_POINT_DISTANCE_ATTENUATION attenuation)
(glPointParameterf GL_POINT_SIZE_MIN #i1.0)
(glPointParameterf GL_POINT_SIZE_MAX #i15.0)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glColor3f 0.95 0.36 0.02)
   (glBegin GL_POINTS)
   (for-each (lambda (i)
         (define x (* (sin i) (* i 0.07)))
         (define y (* (cos i) (* i 0.07)))
         (define z (/ i 13.1946891451))
         (glVertex3f x y z))
      (iota 42 0 0.31415926535))
   (glEnd)

))
