#!/usr/bin/env ol

; let's preconfigure (lib gl)
(import
   (lib gl config))
(config 'set 'splash-text "Loading application resources...")

; create OpenGL window
(import (lib gl))
(gl:set-window-title "1. Creating an OpenGL Window")

; and now we can start long loading of all required libraries
(wait 5000) ; let's simulate 5 second loading delay

; import OpenGL functions
(import (OpenGL 1.0))

; global init
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT) ))
