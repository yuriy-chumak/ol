; OpenGL 4.1 (2010)

(define-library (OpenGL version-4-1)
   (export
      (exports (OpenGL version-4-0))
    GL_VERSION_4_1
   )

   (import
      (r5rs base) (owl io)
      (OpenGL version-4-0))
   (begin
   (define GL_VERSION_4_1 1)

   (define $ (dlopen GL_LIBRARY RTLD_LAZY))

))