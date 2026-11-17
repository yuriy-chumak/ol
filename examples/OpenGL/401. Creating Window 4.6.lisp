#!/usr/bin/env ol
; create OpenGL window with a core profile and import OpenGL functions
(import (lib gl 4.6))
(gl:set-window-title "1. Creating an OpenGL 4.6 Window")

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
