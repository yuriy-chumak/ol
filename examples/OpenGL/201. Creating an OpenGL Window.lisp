#!/usr/bin/env ol

; create OpenGL window
(import (lib gl-2))
(gl:set-window-title "1. Creating an OpenGL 2.1 Window")

; import OpenGL functions
; (OpenGL 2.1) already included into (lib gl-2)

; global init
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
