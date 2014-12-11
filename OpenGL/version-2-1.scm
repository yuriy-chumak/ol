; OpenGL 2.1 (2006)

(define-library (OpenGL version-2-1)
   (export
      (exports (OpenGL version-2-0))
    GL_VERSION_2_1

  )
  
   (import
      (owl defmac) (owl io)
      (owl pinvoke)
      (OpenGL version-2-0))
   (begin

(define    GL_VERSION_2_1    1)
(define % (dlopen "opengl32" 0))

))