#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

; no more steps required,
; just wait for window closing
