; OpenGL 3.3 (2010)

(define-library (OpenGL version-3-3)
(export

   GL_VERSION_3_3
   
   (exports (OpenGL version-3-2)))

(import (scheme core)
   (OpenGL version-3-2))

(begin
   (define GL_VERSION_3_3 1)


))