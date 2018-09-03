#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; we will use OpenGL version 1.0
(import (OpenGL version-1-0))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.11 0.11 0.11 1)

; draw loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))

; no more steps required,
; just wait for window closing

; BTW, you can start ol session, execute this file using ',load "this-file-name"' command
; and still have interactive console (repl) while OpenGL window renders itself in background.
; Just type '(glClearColor 1 0 0 1)' and you will see that OpenGL window immediately will
; change the background color. Cool, yeh?
