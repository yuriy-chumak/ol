; OpenGL 4.2 (2011)

(import         (OpenGL version-4-1))
(define-library (OpenGL version-4-2)
  (export
    GL_VERSION_4_2

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_2    1)
(define % (dlopen "opengl32" 0))

))