; OpenGL 1.2.1 (1998)

(define-library (OpenGL version-1-2-1)
   (export
      (exports (OpenGL version-1-2))
    GL_VERSION_1_2_1

  )
  
   (import
      (r5rs base) (owl io)
      (owl pinvoke)
      (OpenGL version-1-2))
   (begin

(define    GL_VERSION_1_2_1  1)
(define % (dlopen "opengl32" 0))

))