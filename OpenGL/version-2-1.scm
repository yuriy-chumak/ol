; OpenGL 2.1 (2006)

(define-library (OpenGL version-2-1)
   (export
      (exports (OpenGL version-2-0))
    GL_VERSION_2_1

  )
  
   (import
      (r5rs base) (owl io)
      (OpenGL version-2-0))
   (begin
   (define GL_VERSION_2_1 1)
   
   (define % (dlopen GL_LIBRARY RTLD_LAZY))

))