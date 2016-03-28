; OpenGL 3.3 (2010)

(define-library (OpenGL version-3-3)
   (export
      (exports (OpenGL version-3-2))
    GL_VERSION_3_3
   )

   (import (r5rs core)
      (OpenGL version-3-2))
   (begin
   (define GL_VERSION_3_3 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))