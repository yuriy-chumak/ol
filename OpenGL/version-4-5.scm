; OpenGL 4.5 (2014)

(import         (OpenGL version-4-4))
(define-library (OpenGL version-4-5)
  (export
    GL_VERSION_4_5

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_5    1)
(define % (dlopen "opengl32" 0))

))