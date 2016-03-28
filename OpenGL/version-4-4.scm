; OpenGL 4.4 (2013)

(define-library (OpenGL version-4-4)
   (export
      (exports (OpenGL version-4-3))
    GL_VERSION_4_4
   )

   (import (r5rs core)
      (OpenGL version-4-3))
   (begin
   (define GL_VERSION_4_4 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))