; OpenGL 3.1 (2009)

(import         (OpenGL version-3-0))
(define-library (OpenGL version-3-1)
  (export
    GL_VERSION_3_1

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_3_1    1)
(define % (dlopen "opengl32" 0))

))