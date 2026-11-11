#!/usr/bin/env ol

; create OpenGL window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; set new window size
(gl:set-window-size 1280 720)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
