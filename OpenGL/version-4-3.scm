; OpenGL 4.3 (2012)

(import         (OpenGL version-4-2))
(define-library (OpenGL version-4-3)
  (export
    GL_VERSION_4_3

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_3    1)
(define % (dlopen "opengl32" 0))

))