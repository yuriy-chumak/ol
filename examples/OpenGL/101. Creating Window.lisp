#!/usr/bin/env ol

; create OpenGL window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

; no further action is required,
; just wait until window closes.
