#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 2.1 Window")

(import (OpenGL version-2-1))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)))
