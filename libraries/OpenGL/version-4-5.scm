; OpenGL 4.5 (2014)

(define-library (OpenGL version-4-5)
   (export
      (exports (OpenGL version-4-4))
    GL_VERSION_4_4
   )

   (import (scheme core)
      (OpenGL version-4-4))
   (begin
   (define GL_VERSION_4_5 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))