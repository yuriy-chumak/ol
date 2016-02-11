; OpenGL 3.1 (2009)

(define-library (OpenGL version-3-1)
   (export
      (exports (OpenGL version-3-0))
    GL_VERSION_3_1
   )

   (import
      (r5rs base) (owl io)
      (OpenGL version-3-0))
   (begin
   (define GL_VERSION_3_1 1)
   
   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))