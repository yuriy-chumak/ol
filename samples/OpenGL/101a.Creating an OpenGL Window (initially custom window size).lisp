#!/usr/bin/ol
; Let's preconfigure our (lib gl)
(define-library (lib gl config)
(export config) (import (otus lisp))
(begin
   (define config (list->ff '(
      (width . 1280)
      (height . 720))))))
; end of preconfiguration

(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window (initially custom screen size)")

(import (OpenGL version-1-0))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; draw loop
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)))
