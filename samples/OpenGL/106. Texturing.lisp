#!/usr/bin/ol
(import (lib gl))
(gl:set-window-title "6. Texturing")

(import (OpenGL version-1-1))
(import (lib soil))


; init
(glShadeModel GL_SMOOTH)
(glClearColor 0.1 0.1 0.1 1)
(glOrtho 0 2 0 2 0 2)

(glEnable GL_TEXTURE_2D)
(define id
   (SOIL_load_OGL_texture "media/background.png" SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID 0))

; draw
(gl:set-renderer (lambda (mouse)
   (glClear GL_COLOR_BUFFER_BIT)

   (glEnable GL_TEXTURE_2D)
   (glColor3f 1 1 1)

   (glBindTexture GL_TEXTURE_2D id)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_NEAREST)
   (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_NEAREST)

   (for-each (lambda (xy clamp)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S (car clamp))
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T (cdr clamp))

         (glBegin GL_QUADS)
            (define x (car xy))
            (define y (cdr xy))
            (for-each (lambda (xy st)
                  (glTexCoord2f (car st) (cdr st))
                  (glVertex2f (car xy) (cdr xy)))
               `((,x . ,(+ y 1)) (,(+ x 1) . ,(+ y 1)) (,(+ x 1) . ,y) (,x . ,y))
               '((-0.5 . -0.5) (1.5 . -0.5) (1.5 . 1.5) (-0.5 . 1.5)))
         (glEnd))
      (list (cons 1 1)                 (cons 0 0)               (cons 0 1)                (cons 1 0))
      (list (cons GL_REPEAT GL_REPEAT) (cons GL_CLAMP GL_CLAMP) (cons GL_CLAMP GL_REPEAT) (cons GL_REPEAT GL_CLAMP)))


   ; delimiters
   (glDisable GL_TEXTURE_2D)
   (glColor3f 1 1 1)
   (glBegin GL_LINES)
      (glVertex2f 0 1)
      (glVertex2f 2 1)
      (glVertex2f 1 0)
      (glVertex2f 1 2)
   (glEnd)
))
