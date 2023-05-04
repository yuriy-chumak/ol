#!/usr/bin/env ol
; Let's preconfigure our (lib gl)
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config {
      width 1280
      height 720
   })))
; end of config

(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window (initially custom screen size)")

(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

