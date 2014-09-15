; OpenGL 1.2 (1998)

(import         (OpenGL version-1-1))
(define-library (OpenGL version-1-2)
  (export
    GL_VERSION_1_2

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_1_2    1)
(define % (dlopen "opengl32" 0))

))