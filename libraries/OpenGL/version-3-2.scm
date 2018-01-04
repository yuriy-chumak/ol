; OpenGL 3.2 (2009)

(define-library (OpenGL version-3-2)
   (export
      (exports (OpenGL version-3-1))
    GL_VERSION_3_2
   )

   (import (r5rs core)
      (OpenGL version-3-1))
   (begin
   (define GL_VERSION_3_2 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))