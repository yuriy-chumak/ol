; OpenGL 2.1 (2006)

(import         (OpenGL version-2-0))
(define-library (OpenGL version-2-1)
  (export
    GL_VERSION_2_1

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke))
  (begin

(define GL_VERSION_2_1 1)

(define opengl32 (dlopen "opengl32" 0))

))