; OpenGL 1.0 (1992)

; OpenGL core profile implementation
(define-library (OpenGL version-1-0)
  (export
    GL_VERSION_1_0
    GL_LIBRARY

    glCullFace  ; GLenum mode
      ;GL_FRONT ; mode
      ;GL_BACK
      ;GL_FRONT_AND_BACK
    glFrontFace ; GLenum direction
       GL_CW ; direction
       GL_CCW
    glHint      ; GLenum target, GLenum mode
       GL_DONT_CARE ; mode
       GL_FASTEST
       GL_NICEST
      ;GL_PERSPECTIVE_CORRECTION_HINT ; target
      ;GL_POINT_SMOOTH_HINT
      ;GL_LINE_SMOOTH_HINT
      ;GL_POLYGON_SMOOTH_HINT
      ;GL_FOG_HINT
      ;GL_PHONG_HINT
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
    glGetString
       GL_VENDOR
       GL_RENDERER
       GL_VERSION
       GL_EXTENSIONS
;GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
;GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
;GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
;GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
;GLAPI void APIENTRY glDepthRange (GLdouble near, GLdouble far);

    ; types
    GLvoid
    GLenum
    GLfloat
    GLint
    GLsizei
    GLubyte*
    
    ; internal
    glGetProcAddress ; non standard - owl internal universal function to the bind opengl function
  )
  
  (import
      (owl defmac) (owl io) (owl string)
      (owl pinvoke))
  (begin 

(define GL_LIBRARY
   (case *OS*
     (1 "opengl32.dll") ; windows
     (2 "libGL.so")     ; linux
     (3 "GLKit")))      ; macos, https://developer.apple.com/library/mac/documentation/graphicsimaging/conceptual/OpenGL-MacProgGuide/opengl_intro/opengl_intro.html

(define    GL_VERSION_1_0    1) ; from glcorearb.h
(define % (dlopen GL_LIBRARY 0))
	
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

;BlendingFactorDest
(define GL_ZERO                           0)
(define GL_ONE                            1)
(define GL_SRC_COLOR                      #x0300)
(define GL_ONE_MINUS_SRC_COLOR            #x0301)
(define GL_SRC_ALPHA                      #x0302)
(define GL_ONE_MINUS_SRC_ALPHA            #x0303)
(define GL_DST_ALPHA                      #x0304)
(define GL_ONE_MINUS_DST_ALPHA            #x0305)
;BlendingFactorSrc
(define GL_DST_COLOR                      #x0306)
(define GL_ONE_MINUS_DST_COLOR            #x0307)
(define GL_SRC_ALPHA_SATURATE             #x0308)
;Boolean
(define GL_TRUE                           1)
(define GL_FALSE                          0)
;FrontFace
(define GL_CW                             #x0900)
(define GL_CCW                            #x0901)
;HintMode
(define GL_DONT_CARE                      #x1100)
(define GL_FASTEST                        #x1101)
(define GL_NICEST                         #x1102)

;#define GL_POINT                          0x1B00
;#define GL_LINE                           0x1B01
;#define GL_FILL                           0x1B02


;StringName
(define GL_VENDOR                         #x1F00)
(define GL_RENDERER                       #x1F01)
(define GL_VERSION                        #x1F02)
(define GL_EXTENSIONS                     #x1F03)


(define glCullFace (dlsym % GLvoid "glCullFace" GLenum))
(define glFrontFace (dlsym % GLvoid "glFrontFace" GLenum))
(define glHint (dlsym % GLvoid "glHint" GLenum GLenum))
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
(define glGetString (dlsym % GLubyte* "glGetString" GLenum))
;GLAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, void *pixels);
;GLAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
;GLAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
;GLAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
;GLAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
;GLAPI void APIENTRY glDepthRange (GLdouble near, GLdouble far);

))
