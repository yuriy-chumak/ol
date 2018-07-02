; OpenGL 1.4 (2002)

(define-library (OpenGL version-1-4)
   (export
      (exports (OpenGL version-1-3))
    GL_VERSION_1_4

   )
  
   (import (scheme core)
      (OpenGL version-1-3))
   (begin
   (define GL_VERSION_1_4 1)

   (define % (dlopen GL_LIBRARY RTLD_LAZY))

))