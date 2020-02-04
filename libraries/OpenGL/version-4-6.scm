; OpenGL 4.6 (31 Jul 2017)

(define-library (OpenGL version-4-6)
(export

   GL_VERSION_4_6

   (exports (OpenGL version-4-5)))

(import (scheme core)
   (OpenGL version-4-5))

(begin
   (define GL_VERSION_4_6 1)


))