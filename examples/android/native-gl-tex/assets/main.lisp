#!/usr/bin/env ol

(import (lib gl))
(import (OpenGL 1.1))
(import (lib soil))

(define tex0 (let ((buffer (file->bytevector "testChart.jpg")))
   (SOIL_load_OGL_texture_from_memory buffer (size buffer) SOIL_LOAD_RGBA SOIL_CREATE_NEW_ID SOIL_FLAG_MIPMAPS)))

; draw
(gl:set-renderer (lambda ()
   (glClearColor 0.2 0.2 0.2 1)
   (glClear (vm:ior GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

   (glEnable GL_TEXTURE_2D)
   (glBindTexture GL_TEXTURE_2D tex0)
   (glColor3f 1 1 1)
   (glBegin GL_QUADS)
      (glTexCoord2f 0 0)
      (glVertex2f -1 +1)
      (glTexCoord2f 1 0)
      (glVertex2f +1 +1)
      (glTexCoord2f 1 1)
      (glVertex2f +1 -1)
      (glTexCoord2f 0 1)
      (glVertex2f -1 -1)
   (glEnd)
))
