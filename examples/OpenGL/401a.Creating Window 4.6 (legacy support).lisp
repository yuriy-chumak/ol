#!/usr/bin/env ol

; create OpenGL window with a compat profile and import OpenGL functions
(import (lib gl 4.6 compat))
(gl:set-window-title "1. Creating an OpenGL 4.6 Window")

; global init
(glShadeModel GL_SMOOTH) ; legacy function, should work
(glClearColor 0.3 0.3 0.3 1)

; render loop
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   ; legacy functions, should render triangle
   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd) ))
