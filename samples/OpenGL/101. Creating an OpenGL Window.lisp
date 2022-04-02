#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; use OpenGL version 1.0
(import (OpenGL version-1-0))

; just init
(glClearColor 0.3 0.3 0.3 1)

; rendering loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

; no more steps required,
; just wait for window closing

; BTW, you can start ol session, execute this file using ',load "101. Creating an OpenGL Window.lisp"' command
; and still have interactive console (repl) while OpenGL window renders itself in background.
; Just type '(glClearColor 1 0 0 1)' and you will see that OpenGL window immediately will
; change the background color. Cool, yeah?
