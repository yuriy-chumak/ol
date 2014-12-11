; OpenGL 2.0 (2004)

(define-library (OpenGL version-2-0)
   (export
      (exports (OpenGL version-1-5))
    GL_VERSION_2_0

  )
  
   (import
      (owl defmac) (owl io)
      (owl pinvoke)
      (OpenGL version-1-5))
   (begin

(define    GL_VERSION_2_0    1)
(define % (dlopen "opengl32" 0))

;	using GLchar		= System.Byte;		// char

))