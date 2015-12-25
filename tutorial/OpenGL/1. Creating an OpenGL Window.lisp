#!/usr/bin/ol

(import (lib linux opengl) (owl io))

(gl:run "1. Creating an OpenGL Window" 640 480

; init
(lambda ()
   (glShadeModel GL_SMOOTH)
   (glClearColor 0.11 0.11 0.11 1))
      
; draw
(lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
