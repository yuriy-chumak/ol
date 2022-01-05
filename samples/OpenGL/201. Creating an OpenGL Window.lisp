#!/usr/bin/env ol
(import (lib gl-2))
(gl:set-window-title "1. Creating an OpenGL 2.1 Window")

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)))
