; OpenGL 4.0 (2010)

(define-library (OpenGL version-4-0)
   (export
      (exports (OpenGL version-3-3))
    GL_VERSION_4_0
   )

   (import (scheme core)
      (OpenGL version-3-3))
   (begin
   (define GL_VERSION_4_0 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))