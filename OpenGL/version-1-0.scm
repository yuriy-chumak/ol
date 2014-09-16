; OpenGL 1.0 (1992)

; OpenGL core profile implementation
(define-library (OpenGL version-1-0)
  (export
    GL_VERSION_1_0
    GL_LIBRARY
    
    glGetProcAddress ; non standard - owl universal function to the bind opengl function

    glViewport ; GLint x, GLint y, GLsizei width, GLsizei height

  )
  
  (import
      (owl defmac) (owl io) (owl string)
      (owl pinvoke))
  (begin 

(import (owl os))
(define GL_LIBRARY
   (case *OS*
     (1 "opengl32.dll") ; windows
     (2 "libGL.so")     ; linux
     (3 "GLKit")))      ; macos, https://developer.apple.com/library/mac/documentation/graphicsimaging/conceptual/OpenGL-MacProgGuide/opengl_intro/opengl_intro.html

(define    GL_VERSION_1_0    1) ; from glcorearb.h
(define % (dlopen GL_LIBRARY 0))
;(define GL_VERSION_1_0    1) ; linux version ?
;(define % (dlopen "libGL" 0))
;(define GL_VERSION_1_0 1) ; linux version ?
;(define % (dlopen "GL" 0))
	
; поддержка расширений :
;(define _glGetProcAddress_address
;   (case *OS*
;     (1 (dlsym % type-handle "wglGetProcAddress"))
;     (2 (dlsym % type-handle "glXGetProcAddress"))
;     (3 "GLKit")))
(define GetProcAddress (dlsym % type-handle "wglGetProcAddress" type-string))
     
(define (glGetProcAddress type name . prototype)
   (let ((rtty (cons type prototype))
         (function (GetProcAddress (c-string name)))) ; todo: избавиться от (c-string)
      (lambda args
;        (print "opengl pinvoke: " (c-string name))
         (sys-prim 32 function args rtty))))


;//	Базовая система координат OpenGL: http://www.intuit.ru/department/se/prcsharp/21/
;// Правая. x-направо, y-вверх, z-к себе
;// В исходном состоянии OpenGL камера находится в начале мировых координат, смотрит в
;// отрицательную сторону оси z, направляющий вектор камеры (нормаль) совпадает с осью
;// y (камера стоит на плоскости x0z).
;//	GL_MODELVIEW: Модельные преобразования (модельно-видовая матрица) - применяются к размещению объектов на сцене.
;//	GL_PROJECTION: Видовые преобразования (проекционная матрица) - применяются к размещению и ориентации точки обзора (настройка камеры).
;//	GL_TEXTURE: Текстурные преобразования (текстурная матрица) - применяются для управления текстурами заполнения объектов. (?)

  
(define GLvoid  type-void)  ; void GLvoid
(define GLenum  type-int+)  ; typedef unsigned int GLenum - fix+ значит, что это целое число
(define GLfloat type-float) ; typedef float GLfloat
(define GLint   type-int+)  ; typedef int GLint
(define GLsizei type-int+)  ; typedef int GLsizei
;(define GLbitfield type-fix+);typedef unsigned int GLbitfield;
;typedef double GLdouble;
;(define GLuint  type-fix+)  ;typedef unsigned int GLuint;
;(define GLboolean  type-fix+);typedef unsigned char GLboolean;
;(define GLubyte type-fix+)  ;typedef unsigned char GLubyte;
(define GLubyte* type-string)

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
  (define glGetString       (dlsym % GLubyte* "glGetString" GLenum))
;GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
;GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
;GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
;GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
;GLAPI void APIENTRY glDepthRange (GLdouble near, GLdouble far);
  ; https://www.khronos.org/opengles/sdk/docs/man/xhtml/glViewport.xml
  (define glViewport        (dlsym % GLvoid "glViewport" GLint GLint GLsizei GLsizei))

))
