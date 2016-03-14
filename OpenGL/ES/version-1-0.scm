; https://www.khronos.org/registry/egl/
(define-library (OpenGL ES version-1-0)
   (export
      GL_OES_VERSION_1_0

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
      (r5rs core) (owl io) (owl string)
      (owl pinvoke) (lib platform) (lib x11)
      (owl interop) (owl list))

(begin
   (define GL_OES_VERSION_1_0 1)

   (define GLvoid   type-void)  ; void GLvoid
   (define GLvoid*  type-vector-raw)

   (define GLenum   type-fix+)   ; typedef unsigned int GLenum
   (define GLboolean  type-fix+) ; typedef unsigned char GLboolean
   (define GLbitfield type-fix+) ; typedef unsigned int GLbitfield

   (define GLbyte   type-fix+)   ; typedef signed char
   (define GLshort  type-fix+)   ; typedef short
   (define GLint    type-fix+)   ; typedef int GLint
   (define GLsizei  type-fix+)   ; typedef int GLsizei
   (define GLubyte  type-fix+)   ; typedef unsigned char
   (define GLushort type-fix+)   ; typedef unsigned chort
   (define GLuint   type-fix+)   ; typedef unsigned int
   (define GLuint*  type-vector-raw)

   (define GLfloat  type-float)  ; typedef float GLfloat
   (define GLclampf type-float)  ; typedef float GLclampf

   (define GLubyte* type-string)

   ; references
   (define GLfloat*  (vm:or GLfloat  #x40))


))