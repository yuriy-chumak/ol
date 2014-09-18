; OpenGL 1.2.1 (1998)

(import         (OpenGL version-1-2))
(define-library (OpenGL version-1-2-1)
  (export
    GL_VERSION_1_2_1

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke)
    (OpenGL version-1-2))
  (begin

(define    GL_VERSION_1_2_1  1)
(define % (dlopen "opengl32" 0))

))