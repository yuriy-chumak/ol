#!/usr/bin/env ol

; create opengl window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; load splash.gif to display as splash screen
(import (file gif))
(define gif (read-gif-stream (file->bytestream "splash.gif")))
(gl:splash-screen (gif 'width) (gif 'height) (gif->rgb24 gif))

; and now we can start long loading of all required libraries

(wait 5000) ; let's simulate 5 second loading delay

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)))
