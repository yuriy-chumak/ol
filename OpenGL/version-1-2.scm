; OpenGL 1.2 (1998)

(define-library (OpenGL version-1-2)
   (export
      (exports (OpenGL version-1-1))
    GL_VERSION_1_2

;    (if (defined? GL_VERSION_1_2_DEPRECATED)
;        glColorTable
;
;    )
   )
  
   (import
      (owl defmac) (owl io)
      (owl pinvoke)
      (OpenGL version-1-1))
   (begin

(define    GL_VERSION_1_2    1)
(define % (dlopen "opengl32" 0))

))