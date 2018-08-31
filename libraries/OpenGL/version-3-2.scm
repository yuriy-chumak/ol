; OpenGL 3.2 (2009)

(define-library (OpenGL version-3-2)
(export

   GL_VERSION_3_2
   
   (exports (OpenGL version-3-1)))

(import (scheme core)
   (OpenGL version-3-1))

(begin
   (define GL_VERSION_3_2 1)


))