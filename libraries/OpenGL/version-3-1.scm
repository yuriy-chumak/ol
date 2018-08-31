; OpenGL 3.1 (2009)

(define-library (OpenGL version-3-1)
(export

   GL_VERSION_3_1

   (exports (OpenGL version-3-0)))

(import (scheme core)
   (OpenGL version-3-0))

(begin
   (define GL_VERSION_3_1 1)


))