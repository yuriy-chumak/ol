; OpenGL 4.0 (2010)

(import         (OpenGL version-3-3))
(define-library (OpenGL version-4-0)
  (export
    GL_VERSION_4_0

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_0    1)
(define % (dlopen "opengl32" 0))

))