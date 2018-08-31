; OpenGL 4.1 (2010)

(define-library (OpenGL version-4-1)
(export

   GL_VERSION_4_1
   
   (exports (OpenGL version-4-0)))

(import (scheme core)
   (OpenGL version-4-0))

(begin
   (define GL_VERSION_4_1 1)


))