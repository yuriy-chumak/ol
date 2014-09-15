; OpenGL 1.4 (2002)

(import         (OpenGL version-1-3))
(define-library (OpenGL version-1-4)
  (export
    GL_VERSION_1_4

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_1_4    1)
(define % (dlopen "opengl32" 0))

))