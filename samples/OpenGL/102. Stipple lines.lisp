#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "2. Lines")

(import (OpenGL 1.0))

; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.2 0.2 0.2 1)

; draw
(gl:set-renderer (lambda ()
   (glClear GL_COLOR_BUFFER_BIT)

   (glLoadIdentity)
   (glOrtho 0 1 0 1 0 1)

   (glColor3f #xF2/255 #xE2/255 #x05/255)
   (glLineWidth 2.0)

   (glEnable GL_LINE_STIPPLE)
   (for-each (lambda (t mask x y)
         (glLineStipple t mask)
         (glBegin GL_LINES)
            (glVertex2f 0.5 0.5)
            (glVertex2f x y)
         (glEnd))
      '(     2      2      2                  1)
      '(#x0101 #x00F0 #x1C47 #b1111100110011111)
      '(   0.9    0.5    0.1                0.5)
      '(   0.5    0.9    0.5                0.1)
   )
))

; further reading:
; https://stackoverflow.com/questions/6017176/gllinestipple-deprecated-in-opengl-3-1