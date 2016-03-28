; OpenGL 1.3 (2001)

(define-library (OpenGL version-1-3)
(export
       (exports (OpenGL version-1-2))
    GL_VERSION_1_3

  )
  
   (import (r5rs core)
      (OpenGL version-1-2))

(begin
   (define GL_VERSION_1_3 1)

   (define % (dlopen GL_LIBRARY RTLD_LAZY))

))