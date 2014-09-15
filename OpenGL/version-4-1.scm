; OpenGL 4.1 (2010)

(import         (OpenGL version-4-0))
(define-library (OpenGL version-4-1)
  (export
    GL_VERSION_4_1

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define    GL_VERSION_4_1    1)
(define % (dlopen "opengl32" 0))

))