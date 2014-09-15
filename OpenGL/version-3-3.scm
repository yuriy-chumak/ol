; OpenGL 3.3 (2010)

(import         (OpenGL version-3-2))
(define-library (OpenGL version-3-3)
  (export
    GL_VERSION_3_3

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_3_3    1)
(define % (dlopen "opengl32" 0))

))