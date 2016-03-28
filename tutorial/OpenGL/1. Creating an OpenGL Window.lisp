#!/usr/bin/ol
(import (lib opengl))
(gl:run

   "1. Creating an OpenGL Window"

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))

; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
