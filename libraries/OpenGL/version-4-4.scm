; OpenGL 4.4 (2013)

(define-library (OpenGL version-4-4)
(export

   GL_VERSION_4_4

   (exports (OpenGL version-4-3)))

(import (scheme core)
   (OpenGL version-4-3))

(begin
   (define GL_VERSION_4_4 1)


))