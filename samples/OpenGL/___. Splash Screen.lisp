#!/usr/bin/env ol
(import (lib gl))
(gl:set-window-title "6. Texturing")

(import (OpenGL 1.1))

(import (lib soil))

(glOrtho 0 1 1 0 0 1)
(glEnable GL_TEXTURE_2D)
(define id (SOIL_load_OGL_texture "media/splash.jpg" SOIL_LOAD_RGB SOIL_CREATE_NEW_ID 0))
(glBindTexture GL_TEXTURE_2D id)
(glBegin GL_QUADS)
(for-each (lambda (x y)
      (glTexCoord2f x y)
      (glVertex2f x y))
   '(0 1 1 0)
   '(0 0 1 1))
(glEnd)
(gl:redisplay)
(glDeleteTextures 1 (list id))

; simulate heavy calculations:
(define-values (ss ms) (clock))
(let loop ()
   ; let's wait 3 seconds
   (let*((s2 m2 (clock)))
      (when (< s2 (+ ss 3))
         (sleep 1)
         (loop))))

; init
(glShadeModel GL_SMOOTH)
(glLoadIdentity)
(glClearColor 0.1 0.1 0.1 1)

; draw loop
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
