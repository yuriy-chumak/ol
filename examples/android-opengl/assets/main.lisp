#!/usr/bin/env ol
(print "*path*: " *path*)

(import (lib gl)
   (lib GLU)
   (OpenGL 2.1))

,load "cube.lisp"

; common code
(import (scheme inexact))

; init
(glEnable GL_CULL_FACE)
(glCullFace GL_BACK)
(glEnable GL_DEPTH_TEST)

; draw
(gl:set-renderer (lambda (mouse)
   (define aspect (/ (gl:get-window-width) (gl:get-window-height)))
   (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   ; classical projection matrix
   (glMatrixMode GL_PROJECTION)
   (glLoadIdentity)
   (gluPerspective 45 aspect 0.1 1000)

   (define Y -2)
   (define R -5)

   (glMatrixMode GL_MODELVIEW)
   (glLoadIdentity)
   (gluLookAt 0 0 0
      0 Y R
      0 1 0)

   (define t (/ (mod (time-ms) 6283) #i1000))
   (glTranslatef 0 -3 -8)
   (glRotatef (* t 360/3.14) 0 1 0)
   (cube:draw)
))
