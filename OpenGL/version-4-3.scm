; OpenGL 4.3 (2012)

(define-library (OpenGL version-4-3)
   (export
      (exports (OpenGL version-4-2))
    GL_VERSION_4_3
   )

   (import
      (r5rs base) (owl io)
      (OpenGL version-4-2))
   (begin
   (define GL_VERSION_4_3 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))