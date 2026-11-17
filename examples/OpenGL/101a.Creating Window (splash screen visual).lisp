#!/usr/bin/env ol

; create OpenGL window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; load splash.gif to display as a splash screen
(import
   (file gif)
   (lib gl 2 splash-visual))
(define gif (read-gif-stream (file->bytestream "splash.gif")))
(gl:splash-visual (gif 'width) (gif 'height) (gif->rgb24 gif))

; and now we can start long loading of all required libraries
(wait 5000) ; let's simulate 5 second loading delay

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT) ))
