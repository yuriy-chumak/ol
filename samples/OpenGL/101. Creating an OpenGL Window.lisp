#!/usr/bin/ol
(import (lib opengl))

(gl:set-window-title "1. Creating an OpenGL Window")

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

; wait for window closing
(gl:finish)

