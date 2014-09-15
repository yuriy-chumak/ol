; OpenGL 3.2 (2009)

(import         (OpenGL version-3-1))
(define-library (OpenGL version-3-2)
  (export
    GL_VERSION_3_2

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_3_2    1)
(define % (dlopen "opengl32" 0))

))