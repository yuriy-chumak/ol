; OpenGL 1.1 (1997)

(import         (OpenGL version-1-0))
(define-library (OpenGL version-1-1)
  (export
    GL_VERSION_1_1
    
    glLoadIdentity ; void
    glMatrixMode ; GLenum mode
       GL_PROJECTION
       GL_MODELVIEW
       GL_TEXTURE
    glBegin
       GL_POINTS
       GL_LINES
       GL_LINE_LOOP
       GL_LINE_STRIP
       GL_TRIANGLES
       GL_TRIANGLE_STRIP
       GL_TRIANGLE_FAN
       GL_QUADS
       GL_QUAD_STRIP
       GL_POLYGON
    glEnd
    glShadeModel ; GLenum model
       GL_FLAT
       GL_SMOOTH
    glVertex2f
    glVertex2i
    glVertex3f
    glVertex3i
    
    ; https://www.khronos.org/opengles/sdk/docs/man/xhtml/glViewport.xml
    glViewport ; GLint x, GLint y, GLsizei width, GLsizei height
  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke)
    (OpenGL version-1-0))
  (begin
;  (import (OpenGL version-1-0))

(define    GL_VERSION_1_1    1)
(define % (dlopen "opengl32" 0))

; 1.1
;(define GLbyte  ?) ; typedef signed char GLbyte
;(define GLshort ?) ; typedef short GLshort
;(define GLushort ?); typedef unsigned short GLushort
;typedef float GLclampf
;typedef double GLclampd

;BeginMode
(define GL_POINTS                         #x0000)
(define GL_LINES                          #x0001)
(define GL_LINE_LOOP                      #x0002)
(define GL_LINE_STRIP                     #x0003)
(define GL_TRIANGLES                      #x0004)
(define GL_TRIANGLE_STRIP                 #x0005)
(define GL_TRIANGLE_FAN                   #x0006)
(define GL_QUADS                          #x0007)
(define GL_QUAD_STRIP                     #x0008)
(define GL_POLYGON                        #x0009)
;MatrixMode
(define GL_PROJECTION #x1701)
(define GL_MODELVIEW  #x1700)
(define GL_TEXTURE    #x1702)
;ShadingModel
(define GL_FLAT       #x1D00)
(define GL_SMOOTH     #x1D01)

;WINGDIAPI void APIENTRY glAccum (GLenum op, GLfloat value);
;WINGDIAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
;WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
;WINGDIAPI void APIENTRY glArrayElement (GLint i);
(define glBegin (dlsym % GLvoid "glBegin" GLenum))
;WINGDIAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
;WINGDIAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
;WINGDIAPI void APIENTRY glCallList (GLuint list);
;WINGDIAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const GLvoid *lists);
;WINGDIAPI void APIENTRY glClearAccum (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
;WINGDIAPI void APIENTRY glClearIndex (GLfloat c);
;WINGDIAPI void APIENTRY glClipPlane (GLenum plane, const GLdouble *equation);
;WINGDIAPI void APIENTRY glColor3b (GLbyte red, GLbyte green, GLbyte blue);
;WINGDIAPI void APIENTRY glColor3bv (const GLbyte *v);
;WINGDIAPI void APIENTRY glColor3d (GLdouble red, GLdouble green, GLdouble blue);
;WINGDIAPI void APIENTRY glColor3dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glColor3f (GLfloat red, GLfloat green, GLfloat blue);
;WINGDIAPI void APIENTRY glColor3fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glColor3i (GLint red, GLint green, GLint blue);
;WINGDIAPI void APIENTRY glColor3iv (const GLint *v);
;WINGDIAPI void APIENTRY glColor3s (GLshort red, GLshort green, GLshort blue);
;WINGDIAPI void APIENTRY glColor3sv (const GLshort *v);
;WINGDIAPI void APIENTRY glColor3ub (GLubyte red, GLubyte green, GLubyte blue);
;WINGDIAPI void APIENTRY glColor3ubv (const GLubyte *v);
;WINGDIAPI void APIENTRY glColor3ui (GLuint red, GLuint green, GLuint blue);
;WINGDIAPI void APIENTRY glColor3uiv (const GLuint *v);
;WINGDIAPI void APIENTRY glColor3us (GLushort red, GLushort green, GLushort blue);
;WINGDIAPI void APIENTRY glColor3usv (const GLushort *v);
;WINGDIAPI void APIENTRY glColor4b (GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha);
;WINGDIAPI void APIENTRY glColor4bv (const GLbyte *v);
;WINGDIAPI void APIENTRY glColor4d (GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha);
;WINGDIAPI void APIENTRY glColor4dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glColor4f (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
;WINGDIAPI void APIENTRY glColor4fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glColor4i (GLint red, GLint green, GLint blue, GLint alpha);
;WINGDIAPI void APIENTRY glColor4iv (const GLint *v);
;WINGDIAPI void APIENTRY glColor4s (GLshort red, GLshort green, GLshort blue, GLshort alpha);
;WINGDIAPI void APIENTRY glColor4sv (const GLshort *v);
;WINGDIAPI void APIENTRY glColor4ub (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);
;WINGDIAPI void APIENTRY glColor4ubv (const GLubyte *v);
;WINGDIAPI void APIENTRY glColor4ui (GLuint red, GLuint green, GLuint blue, GLuint alpha);
;WINGDIAPI void APIENTRY glColor4uiv (const GLuint *v);
;WINGDIAPI void APIENTRY glColor4us (GLushort red, GLushort green, GLushort blue, GLushort alpha);
;WINGDIAPI void APIENTRY glColor4usv (const GLushort *v);
;WINGDIAPI void APIENTRY glColorMaterial (GLenum face, GLenum mode);
;WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
;WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
;WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
;WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
;WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glDeleteLists (GLuint list, GLsizei range);
;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
;WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
;WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
;WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
;WINGDIAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
;WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glEdgeFlagv (const GLboolean *flag);
;WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
(define glEnd (dlsym % GLvoid "glEnd"))
;WINGDIAPI void APIENTRY glEndList (void);
;WINGDIAPI void APIENTRY glEvalCoord1d (GLdouble u);
;WINGDIAPI void APIENTRY glEvalCoord1dv (const GLdouble *u);
;WINGDIAPI void APIENTRY glEvalCoord1f (GLfloat u);
;WINGDIAPI void APIENTRY glEvalCoord1fv (const GLfloat *u);
;WINGDIAPI void APIENTRY glEvalCoord2d (GLdouble u, GLdouble v);
;WINGDIAPI void APIENTRY glEvalCoord2dv (const GLdouble *u);
;WINGDIAPI void APIENTRY glEvalCoord2f (GLfloat u, GLfloat v);
;WINGDIAPI void APIENTRY glEvalCoord2fv (const GLfloat *u);
;WINGDIAPI void APIENTRY glEvalMesh1 (GLenum mode, GLint i1, GLint i2);
;WINGDIAPI void APIENTRY glEvalMesh2 (GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2);
;WINGDIAPI void APIENTRY glEvalPoint1 (GLint i);
;WINGDIAPI void APIENTRY glEvalPoint2 (GLint i, GLint j);
;WINGDIAPI void APIENTRY glFeedbackBuffer (GLsizei size, GLenum type, GLfloat *buffer);
;WINGDIAPI void APIENTRY glFogf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glFogfv (GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glFogi (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glFogiv (GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glFrustum (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
;WINGDIAPI GLuint APIENTRY glGenLists (GLsizei range);
;WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
;WINGDIAPI void APIENTRY glGetClipPlane (GLenum plane, GLdouble *equation);
;WINGDIAPI void APIENTRY glGetLightfv (GLenum light, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetLightiv (GLenum light, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetMapdv (GLenum target, GLenum query, GLdouble *v);
;WINGDIAPI void APIENTRY glGetMapfv (GLenum target, GLenum query, GLfloat *v);
;WINGDIAPI void APIENTRY glGetMapiv (GLenum target, GLenum query, GLint *v);
;WINGDIAPI void APIENTRY glGetMaterialfv (GLenum face, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetMaterialiv (GLenum face, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetPixelMapfv (GLenum map, GLfloat *values);
;WINGDIAPI void APIENTRY glGetPixelMapuiv (GLenum map, GLuint *values);
;WINGDIAPI void APIENTRY glGetPixelMapusv (GLenum map, GLushort *values);
;WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
;WINGDIAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
;WINGDIAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetTexGendv (GLenum coord, GLenum pname, GLdouble *params);
;WINGDIAPI void APIENTRY glGetTexGenfv (GLenum coord, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexGeniv (GLenum coord, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glIndexMask (GLuint mask);
;WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glIndexd (GLdouble c);
;WINGDIAPI void APIENTRY glIndexdv (const GLdouble *c);
;WINGDIAPI void APIENTRY glIndexf (GLfloat c);
;WINGDIAPI void APIENTRY glIndexfv (const GLfloat *c);
;WINGDIAPI void APIENTRY glIndexi (GLint c);
;WINGDIAPI void APIENTRY glIndexiv (const GLint *c);
;WINGDIAPI void APIENTRY glIndexs (GLshort c);
;WINGDIAPI void APIENTRY glIndexsv (const GLshort *c);
;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
;WINGDIAPI void APIENTRY glInitNames (void);
;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI GLboolean APIENTRY glIsList (GLuint list);
;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
;WINGDIAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLightModeli (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightModeliv (GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLighti (GLenum light, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightiv (GLenum light, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
;WINGDIAPI void APIENTRY glListBase (GLuint base);
(define glLoadIdentity (dlsym % GLvoid "glLoadIdentity"))
;WINGDIAPI void APIENTRY glLoadMatrixd (const GLdouble *m);
;WINGDIAPI void APIENTRY glLoadMatrixf (const GLfloat *m);
;WINGDIAPI void APIENTRY glLoadName (GLuint name);
;WINGDIAPI void APIENTRY glMap1d (GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points);
;WINGDIAPI void APIENTRY glMap1f (GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points);
;WINGDIAPI void APIENTRY glMap2d (GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points);
;WINGDIAPI void APIENTRY glMap2f (GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points);
;WINGDIAPI void APIENTRY glMapGrid1d (GLint un, GLdouble u1, GLdouble u2);
;WINGDIAPI void APIENTRY glMapGrid1f (GLint un, GLfloat u1, GLfloat u2);
;WINGDIAPI void APIENTRY glMapGrid2d (GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2);
;WINGDIAPI void APIENTRY glMapGrid2f (GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2);
;WINGDIAPI void APIENTRY glMaterialf (GLenum face, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glMaterialfv (GLenum face, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glMateriali (GLenum face, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glMaterialiv (GLenum face, GLenum pname, const GLint *params);
(define glMatrixMode (dlsym % GLvoid "glMatrixMode" GLenum))
;WINGDIAPI void APIENTRY glMultMatrixd (const GLdouble *m);
;WINGDIAPI void APIENTRY glMultMatrixf (const GLfloat *m);
;WINGDIAPI void APIENTRY glNewList (GLuint list, GLenum mode);
;WINGDIAPI void APIENTRY glNormal3b (GLbyte nx, GLbyte ny, GLbyte nz);
;WINGDIAPI void APIENTRY glNormal3bv (const GLbyte *v);
;WINGDIAPI void APIENTRY glNormal3d (GLdouble nx, GLdouble ny, GLdouble nz);
;WINGDIAPI void APIENTRY glNormal3dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glNormal3f (GLfloat nx, GLfloat ny, GLfloat nz);
;WINGDIAPI void APIENTRY glNormal3fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glNormal3i (GLint nx, GLint ny, GLint nz);
;WINGDIAPI void APIENTRY glNormal3iv (const GLint *v);
;WINGDIAPI void APIENTRY glNormal3s (GLshort nx, GLshort ny, GLshort nz);
;WINGDIAPI void APIENTRY glNormal3sv (const GLshort *v);
;WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glOrtho (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
;WINGDIAPI void APIENTRY glPassThrough (GLfloat token);
;WINGDIAPI void APIENTRY glPixelMapfv (GLenum map, GLsizei mapsize, const GLfloat *values);
;WINGDIAPI void APIENTRY glPixelMapuiv (GLenum map, GLsizei mapsize, const GLuint *values);
;WINGDIAPI void APIENTRY glPixelMapusv (GLenum map, GLsizei mapsize, const GLushort *values);
;WINGDIAPI void APIENTRY glPixelTransferf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glPixelTransferi (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glPixelZoom (GLfloat xfactor, GLfloat yfactor);
;WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;WINGDIAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
;WINGDIAPI void APIENTRY glPopAttrib (void);
;WINGDIAPI void APIENTRY glPopClientAttrib (void);
;WINGDIAPI void APIENTRY glPopMatrix (void);
;WINGDIAPI void APIENTRY glPopName (void);
;WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
;WINGDIAPI void APIENTRY glPushAttrib (GLbitfield mask);
;WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
;WINGDIAPI void APIENTRY glPushMatrix (void);
;WINGDIAPI void APIENTRY glPushName (GLuint name);
;WINGDIAPI void APIENTRY glRasterPos2d (GLdouble x, GLdouble y);
;WINGDIAPI void APIENTRY glRasterPos2dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glRasterPos2f (GLfloat x, GLfloat y);
;WINGDIAPI void APIENTRY glRasterPos2fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glRasterPos2i (GLint x, GLint y);
;WINGDIAPI void APIENTRY glRasterPos2iv (const GLint *v);
;WINGDIAPI void APIENTRY glRasterPos2s (GLshort x, GLshort y);
;WINGDIAPI void APIENTRY glRasterPos2sv (const GLshort *v);
;WINGDIAPI void APIENTRY glRasterPos3d (GLdouble x, GLdouble y, GLdouble z);
;WINGDIAPI void APIENTRY glRasterPos3dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glRasterPos3f (GLfloat x, GLfloat y, GLfloat z);
;WINGDIAPI void APIENTRY glRasterPos3fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glRasterPos3i (GLint x, GLint y, GLint z);
;WINGDIAPI void APIENTRY glRasterPos3iv (const GLint *v);
;WINGDIAPI void APIENTRY glRasterPos3s (GLshort x, GLshort y, GLshort z);
;WINGDIAPI void APIENTRY glRasterPos3sv (const GLshort *v);
;WINGDIAPI void APIENTRY glRasterPos4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
;WINGDIAPI void APIENTRY glRasterPos4dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glRasterPos4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
;WINGDIAPI void APIENTRY glRasterPos4fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glRasterPos4i (GLint x, GLint y, GLint z, GLint w);
;WINGDIAPI void APIENTRY glRasterPos4iv (const GLint *v);
;WINGDIAPI void APIENTRY glRasterPos4s (GLshort x, GLshort y, GLshort z, GLshort w);
;WINGDIAPI void APIENTRY glRasterPos4sv (const GLshort *v);
;WINGDIAPI void APIENTRY glRectd (GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2);
;WINGDIAPI void APIENTRY glRectdv (const GLdouble *v1, const GLdouble *v2);
;WINGDIAPI void APIENTRY glRectf (GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2);
;WINGDIAPI void APIENTRY glRectfv (const GLfloat *v1, const GLfloat *v2);
;WINGDIAPI void APIENTRY glRecti (GLint x1, GLint y1, GLint x2, GLint y2);
;WINGDIAPI void APIENTRY glRectiv (const GLint *v1, const GLint *v2);
;WINGDIAPI void APIENTRY glRects (GLshort x1, GLshort y1, GLshort x2, GLshort y2);
;WINGDIAPI void APIENTRY glRectsv (const GLshort *v1, const GLshort *v2);
;WINGDIAPI GLint APIENTRY glRenderMode (GLenum mode);
;WINGDIAPI void APIENTRY glRotated (GLdouble angle, GLdouble x, GLdouble y, GLdouble z);
;WINGDIAPI void APIENTRY glRotatef (GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
;WINGDIAPI void APIENTRY glScaled (GLdouble x, GLdouble y, GLdouble z);
;WINGDIAPI void APIENTRY glScalef (GLfloat x, GLfloat y, GLfloat z);
;WINGDIAPI void APIENTRY glSelectBuffer (GLsizei size, GLuint *buffer);
(define glShadeModel (dlsym % GLvoid "glShadeModel" GLenum))
;WINGDIAPI void APIENTRY glTexCoord1d (GLdouble s);
;WINGDIAPI void APIENTRY glTexCoord1dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glTexCoord1f (GLfloat s);
;WINGDIAPI void APIENTRY glTexCoord1fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glTexCoord1i (GLint s);
;WINGDIAPI void APIENTRY glTexCoord1iv (const GLint *v);
;WINGDIAPI void APIENTRY glTexCoord1s (GLshort s);
;WINGDIAPI void APIENTRY glTexCoord1sv (const GLshort *v);
;WINGDIAPI void APIENTRY glTexCoord2d (GLdouble s, GLdouble t);
;WINGDIAPI void APIENTRY glTexCoord2dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glTexCoord2f (GLfloat s, GLfloat t);
;WINGDIAPI void APIENTRY glTexCoord2fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glTexCoord2i (GLint s, GLint t);
;WINGDIAPI void APIENTRY glTexCoord2iv (const GLint *v);
;WINGDIAPI void APIENTRY glTexCoord2s (GLshort s, GLshort t);
;WINGDIAPI void APIENTRY glTexCoord2sv (const GLshort *v);
;WINGDIAPI void APIENTRY glTexCoord3d (GLdouble s, GLdouble t, GLdouble r);
;WINGDIAPI void APIENTRY glTexCoord3dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glTexCoord3f (GLfloat s, GLfloat t, GLfloat r);
;WINGDIAPI void APIENTRY glTexCoord3fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glTexCoord3i (GLint s, GLint t, GLint r);
;WINGDIAPI void APIENTRY glTexCoord3iv (const GLint *v);
;WINGDIAPI void APIENTRY glTexCoord3s (GLshort s, GLshort t, GLshort r);
;WINGDIAPI void APIENTRY glTexCoord3sv (const GLshort *v);
;WINGDIAPI void APIENTRY glTexCoord4d (GLdouble s, GLdouble t, GLdouble r, GLdouble q);
;WINGDIAPI void APIENTRY glTexCoord4dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glTexCoord4f (GLfloat s, GLfloat t, GLfloat r, GLfloat q);
;WINGDIAPI void APIENTRY glTexCoord4fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glTexCoord4i (GLint s, GLint t, GLint r, GLint q);
;WINGDIAPI void APIENTRY glTexCoord4iv (const GLint *v);
;WINGDIAPI void APIENTRY glTexCoord4s (GLshort s, GLshort t, GLshort r, GLshort q);
;WINGDIAPI void APIENTRY glTexCoord4sv (const GLshort *v);
;WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glTexEnvf (GLenum target, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glTexEnvfv (GLenum target, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glTexEnvi (GLenum target, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glTexEnviv (GLenum target, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glTexGend (GLenum coord, GLenum pname, GLdouble param);
;WINGDIAPI void APIENTRY glTexGendv (GLenum coord, GLenum pname, const GLdouble *params);
;WINGDIAPI void APIENTRY glTexGenf (GLenum coord, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glTexGenfv (GLenum coord, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glTexGeni (GLenum coord, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glTexGeniv (GLenum coord, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTranslated (GLdouble x, GLdouble y, GLdouble z);
;WINGDIAPI void APIENTRY glTranslatef (GLfloat x, GLfloat y, GLfloat z);
;WINGDIAPI void APIENTRY glVertex2d (GLdouble x, GLdouble y);
;WINGDIAPI void APIENTRY glVertex2dv (const GLdouble *v);
(define glVertex2f (dlsym % GLvoid "glVertex2f" GLfloat GLfloat))
;WINGDIAPI void APIENTRY glVertex2fv (const GLfloat *v);
(define glVertex2i (dlsym % GLvoid "glVertex2i" GLint GLint))
;WINGDIAPI void APIENTRY glVertex2iv (const GLint *v);
;WINGDIAPI void APIENTRY glVertex2s (GLshort x, GLshort y);
;WINGDIAPI void APIENTRY glVertex2sv (const GLshort *v);
;WINGDIAPI void APIENTRY glVertex3d (GLdouble x, GLdouble y, GLdouble z);
;WINGDIAPI void APIENTRY glVertex3dv (const GLdouble *v);
(define glVertex3f (dlsym % GLvoid "glVertex3f" GLfloat GLfloat GLfloat))
;WINGDIAPI void APIENTRY glVertex3fv (const GLfloat *v);
(define glVertex3i (dlsym % GLvoid "glVertex3i" GLint GLint GLint))
;WINGDIAPI void APIENTRY glVertex3iv (const GLint *v);
;WINGDIAPI void APIENTRY glVertex3s (GLshort x, GLshort y, GLshort z);
;WINGDIAPI void APIENTRY glVertex3sv (const GLshort *v);
;WINGDIAPI void APIENTRY glVertex4d (GLdouble x, GLdouble y, GLdouble z, GLdouble w);
;WINGDIAPI void APIENTRY glVertex4dv (const GLdouble *v);
;WINGDIAPI void APIENTRY glVertex4f (GLfloat x, GLfloat y, GLfloat z, GLfloat w);
;WINGDIAPI void APIENTRY glVertex4fv (const GLfloat *v);
;WINGDIAPI void APIENTRY glVertex4i (GLint x, GLint y, GLint z, GLint w);
;WINGDIAPI void APIENTRY glVertex4iv (const GLint *v);
;WINGDIAPI void APIENTRY glVertex4s (GLshort x, GLshort y, GLshort z, GLshort w);
;WINGDIAPI void APIENTRY glVertex4sv (const GLshort *v);
;WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
  (define glViewport        (dlsym % GLvoid "glViewport" GLint GLint GLsizei GLsizei))

))
