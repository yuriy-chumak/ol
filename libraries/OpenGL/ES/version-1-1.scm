; https://www.khronos.org/registry/egl/
(define-library (OpenGL ES version-1-0)
   (export
      GL_OES_VERSION_1_1

      ; GL types
      ; https://www.opengl.org/wiki/OpenGL_Type
      GLenum            ; unsigned 32-bit
      GLboolean         ; unsigned 1+-bit (GL_TRUE or GL_FALSE)
      GLbitfield        ; unsigned 32-bit
      GLbyte            ;   signed  8-bit
      GLshort           ;   signed 16-bit
      GLint             ;   signed 32-bit
      GLsizei           ;   signed 32-bit (non negative)
      GLubyte  GLubyte* ; unsigned  8-bit
      GLushort          ; unsigned 16-bit
      GLuint   GLuint*  ; unsigned 32-bit

      GLfloat  GLfloat* ; floating 32-bit
      GLclampf          ; floating 32-bit (clamped to the range [0,1])

      GLvoid   GLvoid*
)

   (import
      (r5rs core) (otus ffi))

(begin
   (define GL_OES_VERSION_1_0 1)

   (define GLvoid   fft-void)  ; void GLvoid
   (define GLvoid*  type-vptr)

   (define GLenum   fft-unsigned-int)
   (define GLboolean  fft-unsigned-char)
   (define GLbitfield fft-unsigned-int)

   (define GLbyte   fft-signed-char)
   (define GLshort  fft-short)
   (define GLint    fft-int)
   (define GLsizei  fft-int)
   (define GLubyte  fft-unsigned-char)
   (define GLushort fft-unsigned-short)
   (define GLuint   fft-unsigned-int)
   (define GLuint*  (fft* GLuint))

   (define GLfloat  fft-float)
   (define GLclampf fft-float)

   (define GLubyte* type-string) ; ?

   ; references
   (define GLfloat*  (fft* GLfloat))


))