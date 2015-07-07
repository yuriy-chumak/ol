; OpenGL 1.5 (2003)

(define-library (OpenGL version-1-5)
   (export
      (exports (OpenGL version-1-4))
    GL_VERSION_1_5

  )
  
   (import
      (r5rs base) (owl io)
      (OpenGL version-1-4))
   (begin
   (define GL_VERSION_1_5 1)
   
   (define % (dlopen GL_LIBRARY RTLD_LAZY))

;	using GLintptr		= System.IntPtr;	// ptrdiff_t
;	using GLsizeiptr	= System.IntPtr;	// ptrdiff_t

))