#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "6. Texturing")

(import (OpenGL version-1-1))
(import (lib soil))


; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.1 0.1 0.1 1)
(glOrtho 0 1 0 1 0 1)

(glEnable GL_TEXTURE_2D)
(define id
   (SOIL_load_OGL_texture (c-string "media/background.png") SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glBindTexture GL_TEXTURE_2D id)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)
   (glBegin GL_QUADS)
      (for-each (lambda (xy st)
            (glTexCoord2f (car st) (cdr st))
            (glVertex2f (car xy) (cdr xy)))
         '((0 . 1) (1 . 1) (1 . 0) (0 . 0))
         '((0 . 0) (1 . 0) (1 . 1) (0 . 1)))
   (glEnd)
))
