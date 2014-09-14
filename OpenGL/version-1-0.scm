; OpenGL 1.0 (1992)

; OpenGL core profile implementation
(define-library (OpenGL version-1-0)
  (export
    GL_VERSION_1_0

    glViewport
  )
  
  (import
      (owl defmac) (owl io)
      (owl pinvoke))
  (begin 

(define GL_VERSION_1_0 1) ; from glcorearb.h
  
(define GLvoid 48) ; type-void
;typedef void GLvoid;
;typedef unsigned int GLenum;
;typedef float GLfloat;
;typedef int GLint;
;typedef int GLsizei;
;typedef unsigned int GLbitfield;
;typedef double GLdouble;
;typedef unsigned int GLuint;
;typedef unsigned char GLboolean;
;typedef unsigned char GLubyte;

(define % (dlopen "opengl32" 0))

;GLAPI void APIENTRY glCullFace (GLenum mode);
;GLAPI void APIENTRY glFrontFace (GLenum mode);
;GLAPI void APIENTRY glHint (GLenum target, GLenum mode);
;GLAPI void APIENTRY glLineWidth (GLfloat width);
;GLAPI void APIENTRY glPointSize (GLfloat size);
;GLAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
;GLAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
;GLAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
;GLAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
;GLAPI void APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
;GLAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
;GLAPI void APIENTRY glTexImage1D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const void *pixels);
;GLAPI void APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
;GLAPI void APIENTRY glDrawBuffer (GLenum buf);
;GLAPI void APIENTRY glClear (GLbitfield mask);
;GLAPI void APIENTRY glClearColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
;GLAPI void APIENTRY glClearStencil (GLint s);
;GLAPI void APIENTRY glClearDepth (GLdouble depth);
;GLAPI void APIENTRY glStencilMask (GLuint mask);
;GLAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
;GLAPI void APIENTRY glDepthMask (GLboolean flag);
;GLAPI void APIENTRY glDisable (GLenum cap);
;GLAPI void APIENTRY glEnable (GLenum cap);
;GLAPI void APIENTRY glFinish (void);
;GLAPI void APIENTRY glFlush (void);
;GLAPI void APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
;GLAPI void APIENTRY glLogicOp (GLenum opcode);
;GLAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
;GLAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
;GLAPI void APIENTRY glDepthFunc (GLenum func);
;GLAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
;GLAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
;GLAPI void APIENTRY glReadBuffer (GLenum src);
;GLAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
;GLAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *data);
;GLAPI void APIENTRY glGetDoublev (GLenum pname, GLdouble *data);
;GLAPI GLenum APIENTRY glGetError (void);
;GLAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *data);
;GLAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *data);
;GLAPI const GLubyte *APIENTRY glGetString (GLenum name);
;GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
;GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
;GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
;GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
;GLAPI void APIENTRY glDepthRange (GLdouble near, GLdouble far);
;GLAPI void APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);
  (define glViewport        (dlsym GLvoid % "glViewport"))

))
