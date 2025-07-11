#!/usr/bin/env ol

(import (lib gl))
(import (OpenGL 1.0))

; draw
(gl:set-renderer (lambda ()
   (glClearColor 0 1 0 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glBegin GL_QUADS)
      (glColor3f 1 0 0)
      (glVertex2f -0.6 +0.6)
      (glColor3f 0 1 1)
      (glVertex2f +0.6 +0.6)
      (glColor3f 0 0 1)
      (glVertex2f +0.6 -0.6)
      (glColor3f 0 1 1)
      (glVertex2f -0.6 -0.6)
   (glEnd)
))
