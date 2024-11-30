#!/usr/bin/env ol

; let's preconfigure our (lib gl)
; this feature requires latest Ol
(define-config (lib gl) {
   'width 1280
   'height 720
})

; create opengl window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
