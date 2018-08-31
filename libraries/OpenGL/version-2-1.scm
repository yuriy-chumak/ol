; OpenGL 2.1 (2006)

(define-library (OpenGL version-2-1)
(export

    GL_VERSION_2_1

   (exports (OpenGL version-2-0)))
  
(import (scheme core)
   (OpenGL version-2-0))

(begin
   (define GL_VERSION_2_1 1)

))