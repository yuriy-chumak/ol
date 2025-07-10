#!/usr/bin/env ol

; create OpenGL window and import OpenGL functions
(import (lib gl 1.0))
(gl:set-window-title "1. Creating an OpenGL Window")

; global init
(glClearColor 0.3 0.3 0.3 1)

; set new window size
(gl:set-window-size 1280 720)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
