#!/usr/bin/env ol
(import (lib gl-2))
(gl:set-window-title "1. Creating an OpenGL 2.1 Window")

(import (OpenGL 2.1))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
