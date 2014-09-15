; OpenGL 4.4 (2013)

(import         (OpenGL version-4-3))
(define-library (OpenGL version-4-4)
  (export
    GL_VERSION_4_4

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_4    1)
(define % (dlopen "opengl32" 0))

))