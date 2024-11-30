#!/usr/bin/env ol

(import (lib gl))
(gl:set-window-title "2. Drawing simple triangle")

(import (OpenGL 1.0))

; global init
(glShadeModel GL_SMOOTH)
(glClearColor 0.3 0.3 0.3 1)

; render pass
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glBegin GL_TRIANGLES)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 -0.6)

      (glColor3f 0 1 0)
      (glVertex2f +0.6 -0.6)

      (glColor3f 0 0 1)
      (glVertex2f -0.0 +0.7)
   (glEnd) ))
