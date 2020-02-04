; OpenGL 4.3 (6 Aug 2012)

(define-library (OpenGL version-4-3)
(export

   GL_VERSION_4_3

   (exports (OpenGL version-4-2)))

(import (scheme core)
   (OpenGL version-4-2))

(begin
   (define GL_VERSION_4_3 1)


))