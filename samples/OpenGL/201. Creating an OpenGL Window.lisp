#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL 2.0 Window")

(import (OpenGL version-2-0))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
