; OpenGL 1.3 (2001)

(define-library (OpenGL version-1-3)
   (export
      (exports (OpenGL version-1-2-1))
    GL_VERSION_1_3

  )
  
   (import
      (owl defmac) (owl io)
      (owl pinvoke)
      (OpenGL version-1-2-1))
   (begin

(define    GL_VERSION_1_3    1)
(define % (dlopen "opengl32" 0))

))