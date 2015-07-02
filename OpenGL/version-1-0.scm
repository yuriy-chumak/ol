; OpenGL 1.0 (1992)

; OpenGL core profile implementation
(define-library (OpenGL version-1-0)
   (export
    GL_VERSION_1_0
    GL_LIBRARY  ; internal variable

    ; todo: move to the right place
    GL_RGB GL_UNSIGNED_BYTE GL_RGBA

    glGetProcAddress ; non standard. OL internal universal function to the bind opengl function

    ; GL types
      GLvoid
      GLvoid*
      GLenum
      GLboolean
      GLbitfield
      GLbyte
;     GLshort
      GLint
      GLsizei
      GLubyte
;     GLushort
      GLuint
      GLuint*
      
      GLfloat
;     GLclampf
      GLdouble
;     GLclampd

      GLubyte*

;WINGDIAPI void APIENTRY glAccum (GLenum op, GLfloat value);
;WINGDIAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
    glBegin ; void (GLenum mode)
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

;WINGDIAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
    glBlendFunc ; void (GLenum sfactor, GLenum dfactor)
       GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
;WINGDIAPI void APIENTRY glCallList (GLuint list);
;WINGDIAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const GLvoid *lists);
    glClear ; GLbitfield mask
       GL_COLOR_BUFFER_BIT
       GL_ACCUM_BUFFER_BIT
       GL_STENCIL_BUFFER_BIT
       GL_DEPTH_BUFFER_BIT
;WINGDIAPI void APIENTRY glClearAccum (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
    glClearColor ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
;WINGDIAPI void APIENTRY glClearDepth (GLclampd depth);
;WINGDIAPI void APIENTRY glClearIndex (GLfloat c);
;WINGDIAPI void APIENTRY glClearStencil (GLint s);
;WINGDIAPI void APIENTRY glClipPlane (GLenum plane, const GLdouble *equation);
;WINGDIAPI void APIENTRY glColor3b (GLbyte red, GLbyte green, GLbyte blue);
;WINGDIAPI void APIENTRY glColor3bv (const GLbyte *v);
;WINGDIAPI void APIENTRY glColor3d (GLdouble red, GLdouble green, GLdouble blue);
;WINGDIAPI void APIENTRY glColor3dv (const GLdouble *v);
    glColor3f ; void (GLfloat red, GLfloat green, GLfloat blue)
;WINGDIAPI void APIENTRY glColor3fv (const GLfloat *v);
    glColor3i ; void (GLint red, GLint green, GLint blue)
;WINGDIAPI void APIENTRY glColor3iv (const GLint *v);
;WINGDIAPI void APIENTRY glColor3s (GLshort red, GLshort green, GLshort blue);
;WINGDIAPI void APIENTRY glColor3sv (const GLshort *v);
    glColor3ub ; void (GLubyte red, GLubyte green, GLubyte blue)
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
;WINGDIAPI void APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
;WINGDIAPI void APIENTRY glColorMaterial (GLenum face, GLenum mode);
;WINGDIAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
    glCullFace  ; void (GLenum mode)
       GL_FRONT ; mode
       GL_BACK
       GL_FRONT_AND_BACK
;WINGDIAPI void APIENTRY glDeleteLists (GLuint list, GLsizei range);
;WINGDIAPI void APIENTRY glDepthFunc (GLenum func);
;WINGDIAPI void APIENTRY glDepthMask (GLboolean flag);
;WINGDIAPI void APIENTRY glDepthRange (GLclampd zNear, GLclampd zFar);
    glDisable ; void (GLenum)
       GL_TEXTURE_1D GL_TEXTURE_2D
;WINGDIAPI void APIENTRY glDrawBuffer (GLenum mode);
;WINGDIAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
;WINGDIAPI void APIENTRY glEdgeFlagv (const GLboolean *flag);
    glEnable ; void (GLenum)
       GL_TEXTURE_1D GL_TEXTURE_2D GL_BLEND
    glEnd ; void

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
;WINGDIAPI void APIENTRY glFinish (void);
;WINGDIAPI void APIENTRY glFlush (void);
;WINGDIAPI void APIENTRY glFogf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glFogfv (GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glFogi (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glFogiv (GLenum pname, const GLint *params);
    glFrontFace ; void (GLenum direction)
       GL_CW ; direction
       GL_CCW

;WINGDIAPI void APIENTRY glFrustum (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
;WINGDIAPI GLuint APIENTRY glGenLists (GLsizei range);
;WINGDIAPI void APIENTRY glGetBooleanv (GLenum pname, GLboolean *params);
;WINGDIAPI void APIENTRY glGetClipPlane (GLenum plane, GLdouble *equation);
;WINGDIAPI void APIENTRY glGetDoublev (GLenum pname, GLdouble *params);
;WINGDIAPI GLenum APIENTRY glGetError (void);
;WINGDIAPI void APIENTRY glGetFloatv (GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetIntegerv (GLenum pname, GLint *params);
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
;WINGDIAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
;WINGDIAPI const GLubyte * APIENTRY glGetString (GLenum name);
    glGetString ; GLubyte* (GLenum name)
       GL_VENDOR
       GL_RENDERER
       GL_VERSION
       GL_EXTENSIONS
;WINGDIAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetTexGendv (GLenum coord, GLenum pname, GLdouble *params);
;WINGDIAPI void APIENTRY glGetTexGenfv (GLenum coord, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexGeniv (GLenum coord, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetTexImage (GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels);
;WINGDIAPI void APIENTRY glGetTexLevelParameterfv (GLenum target, GLint level, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexLevelParameteriv (GLenum target, GLint level, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
    glHint      ; void (GLenum target, GLenum mode)
       GL_DONT_CARE ; mode
       GL_FASTEST
       GL_NICEST
       GL_PERSPECTIVE_CORRECTION_HINT ; target
       GL_POINT_SMOOTH_HINT
       GL_LINE_SMOOTH_HINT
       GL_POLYGON_SMOOTH_HINT
       GL_FOG_HINT
       ; GL_PHONG_HINT (no this constant defined, maybe bug in original headers)

;WINGDIAPI void APIENTRY glIndexMask (GLuint mask);
;WINGDIAPI void APIENTRY glIndexd (GLdouble c);
;WINGDIAPI void APIENTRY glIndexdv (const GLdouble *c);
;WINGDIAPI void APIENTRY glIndexf (GLfloat c);
;WINGDIAPI void APIENTRY glIndexfv (const GLfloat *c);
;WINGDIAPI void APIENTRY glIndexi (GLint c);
;WINGDIAPI void APIENTRY glIndexiv (const GLint *c);
;WINGDIAPI void APIENTRY glIndexs (GLshort c);
;WINGDIAPI void APIENTRY glIndexsv (const GLshort *c);
;WINGDIAPI void APIENTRY glInitNames (void);
;WINGDIAPI GLboolean APIENTRY glIsEnabled (GLenum cap);
;WINGDIAPI GLboolean APIENTRY glIsList (GLuint list);
;WINGDIAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLightModeli (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightModeliv (GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLighti (GLenum light, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightiv (GLenum light, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
    glLineWidth ; void (GLfloat width)
;WINGDIAPI void APIENTRY glListBase (GLuint base);
    glLoadIdentity ; void (void);
;WINGDIAPI void APIENTRY glLoadMatrixd (const GLdouble *m);
;WINGDIAPI void APIENTRY glLoadMatrixf (const GLfloat *m);
;WINGDIAPI void APIENTRY glLoadName (GLuint name);
;WINGDIAPI void APIENTRY glLogicOp (GLenum opcode);
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
    glMatrixMode ; void (GLenum mode)
       GL_PROJECTION
       GL_MODELVIEW
       GL_TEXTURE
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
    glOrtho ; void (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
;WINGDIAPI void APIENTRY glPassThrough (GLfloat token);
;WINGDIAPI void APIENTRY glPixelMapfv (GLenum map, GLsizei mapsize, const GLfloat *values);
;WINGDIAPI void APIENTRY glPixelMapuiv (GLenum map, GLsizei mapsize, const GLuint *values);
;WINGDIAPI void APIENTRY glPixelMapusv (GLenum map, GLsizei mapsize, const GLushort *values);
;WINGDIAPI void APIENTRY glPixelStoref (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glPixelStorei (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glPixelTransferf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glPixelTransferi (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glPixelZoom (GLfloat xfactor, GLfloat yfactor);
    glPointSize ; void (GLfloat size)
;WINGDIAPI void APIENTRY glPolygonMode (GLenum face, GLenum mode);
;WINGDIAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
;WINGDIAPI void APIENTRY glPopAttrib (void);
;WINGDIAPI void APIENTRY glPopMatrix (void);
;WINGDIAPI void APIENTRY glPopName (void);
;WINGDIAPI void APIENTRY glPushAttrib (GLbitfield mask);
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
;WINGDIAPI void APIENTRY glReadBuffer (GLenum mode);
;WINGDIAPI void APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels);
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
;WINGDIAPI void APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glSelectBuffer (GLsizei size, GLuint *buffer);
    glShadeModel ; void (GLenum model)
       GL_FLAT
       GL_SMOOTH

;WINGDIAPI void APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
;WINGDIAPI void APIENTRY glStencilMask (GLuint mask);
;WINGDIAPI void APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
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
    glTexCoord2f ; void (GLfloat GLfloat)
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
; todo: maybe this must be in opengl 1.1 ?
    glTexImage1D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels)
    glTexImage2D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels)
;WINGDIAPI void APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
    glTexParameteri ; void (GLenum target, GLenum pname, GLint param)
       GL_TEXTURE_MAG_FILTER GL_TEXTURE_MIN_FILTER
       GL_NEAREST GL_LINEAR
;WINGDIAPI void APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glTranslated (GLdouble x, GLdouble y, GLdouble z);
    glTranslatef ; void (GLfloat x, GLfloat y, GLfloat z)
;WINGDIAPI void APIENTRY glVertex2d (GLdouble x, GLdouble y);
    glVertex2d ; void (GLdouble x, GLdouble y)
;WINGDIAPI void APIENTRY glVertex2dv (const GLdouble *v);
    glVertex2f ; void (GLfloat x, GLfloat y)
;WINGDIAPI void APIENTRY glVertex2fv (const GLfloat *v);
    glVertex2i ; void (GLint x, GLint y)
;WINGDIAPI void APIENTRY glVertex2iv (const GLint *v);
;WINGDIAPI void APIENTRY glVertex2s (GLshort x, GLshort y);
;WINGDIAPI void APIENTRY glVertex2sv (const GLshort *v);
    glVertex3d ; void (GLdouble x, GLdouble y, GLdouble z)
;WINGDIAPI void APIENTRY glVertex3dv (const GLdouble *v);
    glVertex3f ; void (GLfloat x, GLfloat y, GLfloat z)
;WINGDIAPI void APIENTRY glVertex3fv (const GLfloat *v);
    glVertex3i ; void (GLint x, GLint y, GLint z)
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
    ; https://www.khronos.org/opengles/sdk/docs/man/xhtml/glViewport.xml
    glViewport ; void (GLint x, GLint y, GLsizei width, GLsizei height)
  )
  
; ============================================================================
; == implementation ==========================================================
   (import
      (r5rs base) (owl io) (owl string)
      (owl pinvoke))
   (begin

(define    GL_VERSION_1_0    1)

; done: сделать определение нужной библиотеки самодостаточным, без всяких *OS*
(define uname (syscall 63 0 0 0)) ; internal
(define GL_LIBRARY
   (if (string-eq? (car uname) "Windows")  "opengl32"
   (if (string-eq? (car uname) "Linux")    "libGL.so"
   )))

(define % (dlopen GL_LIBRARY RTLD_LAZY))
	
; поддержка расширений :
;(define _glGetProcAddress_address
;   (case *OS*
;     (1 (dlsym % type-handle "wglGetProcAddress"))
;     (2 (dlsym % type-handle "glXGetProcAddress"))
;     (3 "GLKit")))
(define GetProcAddress ; internal function
   (dlsym % type-port
      (if (string-eq? (car uname) "Windows")  "wglGetProcAddress"
      (if (string-eq? (car uname) "Linux")    "glXGetProcAddress"))
   type-string))

(define (glGetProcAddress type name . prototype)
   (let ((rtty (cons type prototype))
         (function (GetProcAddress (c-string name)))) ; todo: избавиться от (c-string)
      (if function
      (lambda args
;        (print "opengl pinvoke: " (c-string name))
         (exec pinvoke function rtty args)))))

;//	Базовая система координат OpenGL: http://www.intuit.ru/department/se/prcsharp/21/
;// Правая. x-направо, y-вверх, z-к себе
;// В исходном состоянии OpenGL камера находится в начале мировых координат, смотрит в
;// отрицательную сторону оси z, направляющий вектор камеры (нормаль) совпадает с осью
;// y (камера стоит на плоскости x0z).
;//	GL_MODELVIEW: Модельные преобразования (модельно-видовая матрица) - применяются к размещению объектов на сцене.
;//	GL_PROJECTION: Видовые преобразования (проекционная матрица) - применяются к размещению и ориентации точки обзора (настройка камеры).
;//	GL_TEXTURE: Текстурные преобразования (текстурная матрица) - применяются для управления текстурами заполнения объектов. (?)


(define GLvoid  type-void)  ; void GLvoid
(define GLenum  type-int+)  ; typedef unsigned int GLenum - int+ значит, что это целое число, fix+ значит, что это маленькое число
(define GLboolean  type-fix+) ; typedef unsigned char GLboolean;
(define GLbitfield type-int+) ; typedef unsigned int GLbitfield;

(define GLvoid* type-vector-raw)

(define GLbyte  type-fix+)
(define GLint   type-int+)  ; typedef int GLint
(define GLsizei type-int+)  ; typedef int GLsizei
(define GLubyte type-fix+)
(define GLuint  type-int+)
(define GLuint* type-vector-raw)

(define GLfloat type-float) ; typedef float GLfloat
(define GLdouble type-double) ; typedef double GLdouble;

;GLclampf
;(define GLuint  type-fix+)  ;typedef unsigned int GLuint;
;(define GLubyte type-fix+)  ;typedef unsigned char GLubyte;
(define GLubyte* type-string)


		(define GL_ACCUM #x0100)
		(define GL_LOAD #x0101)
		(define GL_RETURN #x0102)
		(define GL_MULT #x0103)
		(define GL_ADD #x0104)
		(define GL_NEVER #x0200)
		(define GL_LESS #x0201)
		(define GL_EQUAL #x0202)
		(define GL_LEQUAL #x0203)
		(define GL_GREATER #x0204)
		(define GL_NOTEQUAL #x0205)
		(define GL_GEQUAL #x0206)
		(define GL_ALWAYS #x0207)
		(define GL_CURRENT_BIT #x00000001)
		(define GL_POINT_BIT #x00000002)
		(define GL_LINE_BIT #x00000004)
		(define GL_POLYGON_BIT #x00000008)
		(define GL_POLYGON_STIPPLE_BIT #x00000010)
		(define GL_PIXEL_MODE_BIT #x00000020)
		(define GL_LIGHTING_BIT #x00000040)
		(define GL_FOG_BIT #x00000080)
		(define GL_DEPTH_BUFFER_BIT #x00000100)
		(define GL_ACCUM_BUFFER_BIT #x00000200)
		(define GL_STENCIL_BUFFER_BIT #x00000400)
		(define GL_VIEWPORT_BIT #x00000800)
		(define GL_TRANSFORM_BIT #x00001000)
		(define GL_ENABLE_BIT #x00002000)
		(define GL_COLOR_BUFFER_BIT #x00004000)
		(define GL_HINT_BIT #x00008000)
		(define GL_EVAL_BIT #x00010000)
		(define GL_LIST_BIT #x00020000)
		(define GL_TEXTURE_BIT #x00040000)
		(define GL_SCISSOR_BIT #x00080000)
		(define GL_ALL_ATTRIB_BITS #x000FFFFF)
		(define GL_POINTS #x0000)
		(define GL_LINES #x0001)
		(define GL_LINE_LOOP #x0002)
		(define GL_LINE_STRIP #x0003)
		(define GL_TRIANGLES #x0004)
		(define GL_TRIANGLE_STRIP #x0005)
		(define GL_TRIANGLE_FAN #x0006)
		(define GL_QUADS #x0007)
		(define GL_QUAD_STRIP #x0008)
		(define GL_POLYGON #x0009)
		(define GL_ZERO 0)
		(define GL_ONE 1)
		(define GL_SRC_COLOR #x0300)
		(define GL_ONE_MINUS_SRC_COLOR #x0301)
		(define GL_SRC_ALPHA #x0302)
		(define GL_ONE_MINUS_SRC_ALPHA #x0303)
		(define GL_DST_ALPHA #x0304)
		(define GL_ONE_MINUS_DST_ALPHA #x0305)
		(define GL_DST_COLOR #x0306)
		(define GL_ONE_MINUS_DST_COLOR #x0307)
		(define GL_SRC_ALPHA_SATURATE #x0308)
		(define GL_TRUE 1)
		(define GL_FALSE 0)
		(define GL_CLIP_PLANE0 #x3000)
		(define GL_CLIP_PLANE1 #x3001)
		(define GL_CLIP_PLANE2 #x3002)
		(define GL_CLIP_PLANE3 #x3003)
		(define GL_CLIP_PLANE4 #x3004)
		(define GL_CLIP_PLANE5 #x3005)
		(define GL_BYTE #x1400)
		(define GL_UNSIGNED_BYTE #x1401)
		(define GL_SHORT #x1402)
		(define GL_UNSIGNED_SHORT #x1403)
		(define GL_INT #x1404)
		(define GL_UNSIGNED_INT #x1405)
		(define GL_FLOAT #x1406)
		(define GL_2_BYTES #x1407)
		(define GL_3_BYTES #x1408)
		(define GL_4_BYTES #x1409)
		(define GL_NONE 0)
		(define GL_FRONT_LEFT #x0400)
		(define GL_FRONT_RIGHT #x0401)
		(define GL_BACK_LEFT #x0402)
		(define GL_BACK_RIGHT #x0403)
		(define GL_FRONT #x0404)
		(define GL_BACK #x0405)
		(define GL_LEFT #x0406)
		(define GL_RIGHT #x0407)
		(define GL_FRONT_AND_BACK #x0408)
		(define GL_AUX0 #x0409)
		(define GL_AUX1 #x040A)
		(define GL_AUX2 #x040B)
		(define GL_AUX3 #x040C)
		(define GL_NO_ERROR 0)
		(define GL_INVALID_ENUM #x0500)
		(define GL_INVALID_VALUE #x0501)
		(define GL_INVALID_OPERATION #x0502)
		(define GL_STACK_OVERFLOW #x0503)
		(define GL_STACK_UNDERFLOW #x0504)
		(define GL_OUT_OF_MEMORY #x0505)
		(define GL_2D #x0600)
		(define GL_3D #x0601)
		(define GL_3D_COLOR #x0602)
		(define GL_3D_COLOR_TEXTURE #x0603)
		(define GL_4D_COLOR_TEXTURE #x0604)
		(define GL_PASS_THROUGH_TOKEN #x0700)
		(define GL_POINT_TOKEN #x0701)
		(define GL_LINE_TOKEN #x0702)
		(define GL_POLYGON_TOKEN #x0703)
		(define GL_BITMAP_TOKEN #x0704)
		(define GL_DRAW_PIXEL_TOKEN #x0705)
		(define GL_COPY_PIXEL_TOKEN #x0706)
		(define GL_LINE_RESET_TOKEN #x0707)
		(define GL_EXP #x0800)
		(define GL_EXP2 #x0801)
		(define GL_CW #x0900)
		(define GL_CCW #x0901)
		(define GL_COEFF #x0A00)
		(define GL_ORDER #x0A01)
		(define GL_DOMAIN #x0A02)
		(define GL_CURRENT_COLOR #x0B00)
		(define GL_CURRENT_INDEX #x0B01)
		(define GL_CURRENT_NORMAL #x0B02)
		(define GL_CURRENT_TEXTURE_COORDS #x0B03)
		(define GL_CURRENT_RASTER_COLOR #x0B04)
		(define GL_CURRENT_RASTER_INDEX #x0B05)
		(define GL_CURRENT_RASTER_TEXTURE_COORDS #x0B06)
		(define GL_CURRENT_RASTER_POSITION #x0B07)
		(define GL_CURRENT_RASTER_POSITION_VALID #x0B08)
		(define GL_CURRENT_RASTER_DISTANCE #x0B09)
		(define GL_POINT_SMOOTH #x0B10)
		(define GL_POINT_SIZE #x0B11)
		(define GL_POINT_SIZE_RANGE #x0B12)
		(define GL_POINT_SIZE_GRANULARITY #x0B13)
		(define GL_LINE_SMOOTH #x0B20)
		(define GL_LINE_WIDTH #x0B21)
		(define GL_LINE_WIDTH_RANGE #x0B22)
		(define GL_LINE_WIDTH_GRANULARITY #x0B23)
		(define GL_LINE_STIPPLE #x0B24)
		(define GL_LINE_STIPPLE_PATTERN #x0B25)
		(define GL_LINE_STIPPLE_REPEAT #x0B26)
		(define GL_LIST_MODE #x0B30)
		(define GL_MAX_LIST_NESTING #x0B31)
		(define GL_LIST_BASE #x0B32)
		(define GL_LIST_INDEX #x0B33)
		(define GL_POLYGON_MODE #x0B40)
		(define GL_POLYGON_SMOOTH #x0B41)
		(define GL_POLYGON_STIPPLE #x0B42)
		(define GL_EDGE_FLAG #x0B43)
		(define GL_CULL_FACE #x0B44)
		(define GL_CULL_FACE_MODE #x0B45)
		(define GL_FRONT_FACE #x0B46)
		(define GL_LIGHTING #x0B50)
		(define GL_LIGHT_MODEL_LOCAL_VIEWER #x0B51)
		(define GL_LIGHT_MODEL_TWO_SIDE #x0B52)
		(define GL_LIGHT_MODEL_AMBIENT #x0B53)
		(define GL_SHADE_MODEL #x0B54)
		(define GL_COLOR_MATERIAL_FACE #x0B55)
		(define GL_COLOR_MATERIAL_PARAMETER #x0B56)
		(define GL_COLOR_MATERIAL #x0B57)
		(define GL_FOG #x0B60)
		(define GL_FOG_INDEX #x0B61)
		(define GL_FOG_DENSITY #x0B62)
		(define GL_FOG_START #x0B63)
		(define GL_FOG_END #x0B64)
		(define GL_FOG_MODE #x0B65)
		(define GL_FOG_COLOR #x0B66)
		(define GL_DEPTH_RANGE #x0B70)
		(define GL_DEPTH_TEST #x0B71)
		(define GL_DEPTH_WRITEMASK #x0B72)
		(define GL_DEPTH_CLEAR_VALUE #x0B73)
		(define GL_DEPTH_FUNC #x0B74)
		(define GL_ACCUM_CLEAR_VALUE #x0B80)
		(define GL_STENCIL_TEST #x0B90)
		(define GL_STENCIL_CLEAR_VALUE #x0B91)
		(define GL_STENCIL_FUNC #x0B92)
		(define GL_STENCIL_VALUE_MASK #x0B93)
		(define GL_STENCIL_FAIL #x0B94)
		(define GL_STENCIL_PASS_DEPTH_FAIL #x0B95)
		(define GL_STENCIL_PASS_DEPTH_PASS #x0B96)
		(define GL_STENCIL_REF #x0B97)
		(define GL_STENCIL_WRITEMASK #x0B98)
		(define GL_MATRIX_MODE #x0BA0)
		(define GL_NORMALIZE #x0BA1)
		(define GL_VIEWPORT #x0BA2)
		(define GL_MODELVIEW_STACK_DEPTH #x0BA3)
		(define GL_PROJECTION_STACK_DEPTH #x0BA4)
		(define GL_MODELVIEW_MATRIX #x0BA6)
		(define GL_TEXTURE_STACK_DEPTH #x0BA5)
		(define GL_PROJECTION_MATRIX #x0BA7)
		(define GL_TEXTURE_MATRIX #x0BA8)
		(define GL_ATTRIB_STACK_DEPTH #x0BB0)
		(define GL_CLIENT_ATTRIB_STACK_DEPTH #x0BB1)
		(define GL_ALPHA_TEST #x0BC0)
		(define GL_ALPHA_TEST_FUNC #x0BC1)
		(define GL_ALPHA_TEST_REF #x0BC2)
		(define GL_DITHER #x0BD0)
		(define GL_BLEND_DST #x0BE0)
		(define GL_BLEND_SRC #x0BE1)
		(define GL_BLEND #x0BE2)
		(define GL_LOGIC_OP_MODE #x0BF0)
		(define GL_INDEX_LOGIC_OP #x0BF1)
		(define GL_COLOR_LOGIC_OP #x0BF2)
		(define GL_AUX_BUFFERS #x0C00)
		(define GL_DRAW_BUFFER #x0C01)
		(define GL_READ_BUFFER #x0C02)
		(define GL_SCISSOR_BOX #x0C10)
		(define GL_SCISSOR_TEST #x0C11)
		(define GL_INDEX_CLEAR_VALUE #x0C20)
		(define GL_INDEX_WRITEMASK #x0C21)
		(define GL_COLOR_CLEAR_VALUE #x0C22)
		(define GL_COLOR_WRITEMASK #x0C23)
		(define GL_INDEX_MODE #x0C30)
		(define GL_RGBA_MODE #x0C31)
		(define GL_DOUBLEBUFFER #x0C32)
		(define GL_STEREO #x0C33)
		(define GL_RENDER_MODE #x0C40)
		(define GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
		(define GL_POINT_SMOOTH_HINT #x0C51)
		(define GL_LINE_SMOOTH_HINT #x0C52)
		(define GL_POLYGON_SMOOTH_HINT #x0C53)
		(define GL_FOG_HINT #x0C54)
		(define GL_TEXTURE_GEN_S #x0C60)
		(define GL_TEXTURE_GEN_T #x0C61)
		(define GL_TEXTURE_GEN_R #x0C62)
		(define GL_TEXTURE_GEN_Q #x0C63)
		(define GL_PIXEL_MAP_I_TO_I #x0C70)
		(define GL_PIXEL_MAP_S_TO_S #x0C71)
		(define GL_PIXEL_MAP_I_TO_R #x0C72)
		(define GL_PIXEL_MAP_I_TO_G #x0C73)
		(define GL_PIXEL_MAP_I_TO_B #x0C74)
		(define GL_PIXEL_MAP_I_TO_A #x0C75)
		(define GL_PIXEL_MAP_R_TO_R #x0C76)
		(define GL_PIXEL_MAP_G_TO_G #x0C77)
		(define GL_PIXEL_MAP_B_TO_B #x0C78)
		(define GL_PIXEL_MAP_A_TO_A #x0C79)
		(define GL_PIXEL_MAP_I_TO_I_SIZE #x0CB0)
		(define GL_PIXEL_MAP_S_TO_S_SIZE #x0CB1)
		(define GL_PIXEL_MAP_I_TO_R_SIZE #x0CB2)
		(define GL_PIXEL_MAP_I_TO_G_SIZE #x0CB3)
		(define GL_PIXEL_MAP_I_TO_B_SIZE #x0CB4)
		(define GL_PIXEL_MAP_I_TO_A_SIZE #x0CB5)
		(define GL_PIXEL_MAP_R_TO_R_SIZE #x0CB6)
		(define GL_PIXEL_MAP_G_TO_G_SIZE #x0CB7)
		(define GL_PIXEL_MAP_B_TO_B_SIZE #x0CB8)
		(define GL_PIXEL_MAP_A_TO_A_SIZE #x0CB9)
		(define GL_UNPACK_SWAP_BYTES #x0CF0)
		(define GL_UNPACK_LSB_FIRST #x0CF1)
		(define GL_UNPACK_ROW_LENGTH #x0CF2)
		(define GL_UNPACK_SKIP_ROWS #x0CF3)
		(define GL_UNPACK_SKIP_PIXELS #x0CF4)
		(define GL_UNPACK_ALIGNMENT #x0CF5)
		(define GL_PACK_SWAP_BYTES #x0D00)
		(define GL_PACK_LSB_FIRST #x0D01)
		(define GL_PACK_ROW_LENGTH #x0D02)
		(define GL_PACK_SKIP_ROWS #x0D03)
		(define GL_PACK_SKIP_PIXELS #x0D04)
		(define GL_PACK_ALIGNMENT #x0D05)
		(define GL_MAP_COLOR #x0D10)
		(define GL_MAP_STENCIL #x0D11)
		(define GL_INDEX_SHIFT #x0D12)
		(define GL_INDEX_OFFSET #x0D13)
		(define GL_RED_SCALE #x0D14)
		(define GL_RED_BIAS #x0D15)
		(define GL_ZOOM_X #x0D16)
		(define GL_ZOOM_Y #x0D17)
		(define GL_GREEN_SCALE #x0D18)
		(define GL_GREEN_BIAS #x0D19)
		(define GL_BLUE_SCALE #x0D1A)
		(define GL_BLUE_BIAS #x0D1B)
		(define GL_ALPHA_SCALE #x0D1C)
		(define GL_ALPHA_BIAS #x0D1D)
		(define GL_DEPTH_SCALE #x0D1E)
		(define GL_DEPTH_BIAS #x0D1F)
		(define GL_MAX_EVAL_ORDER #x0D30)
		(define GL_MAX_LIGHTS #x0D31)
		(define GL_MAX_CLIP_PLANES #x0D32)
		(define GL_MAX_TEXTURE_SIZE #x0D33)
		(define GL_MAX_PIXEL_MAP_TABLE #x0D34)
		(define GL_MAX_ATTRIB_STACK_DEPTH #x0D35)
		(define GL_MAX_MODELVIEW_STACK_DEPTH #x0D36)
		(define GL_MAX_NAME_STACK_DEPTH #x0D37)
		(define GL_MAX_PROJECTION_STACK_DEPTH #x0D38)
		(define GL_MAX_TEXTURE_STACK_DEPTH #x0D39)
		(define GL_MAX_VIEWPORT_DIMS #x0D3A)
		(define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH #x0D3B)
		(define GL_SUBPIXEL_BITS #x0D50)
		(define GL_INDEX_BITS #x0D51)
		(define GL_RED_BITS #x0D52)
		(define GL_GREEN_BITS #x0D53)
		(define GL_BLUE_BITS #x0D54)
		(define GL_ALPHA_BITS #x0D55)
		(define GL_DEPTH_BITS #x0D56)
		(define GL_STENCIL_BITS #x0D57)
		(define GL_ACCUM_RED_BITS #x0D58)
		(define GL_ACCUM_GREEN_BITS #x0D59)
		(define GL_ACCUM_BLUE_BITS #x0D5A)
		(define GL_ACCUM_ALPHA_BITS #x0D5B)
		(define GL_NAME_STACK_DEPTH #x0D70)
		(define GL_AUTO_NORMAL #x0D80)
		(define GL_MAP1_COLOR_4 #x0D90)
		(define GL_MAP1_INDEX #x0D91)
		(define GL_MAP1_NORMAL #x0D92)
		(define GL_MAP1_TEXTURE_COORD_1 #x0D93)
		(define GL_MAP1_TEXTURE_COORD_2 #x0D94)
		(define GL_MAP1_TEXTURE_COORD_3 #x0D95)
		(define GL_MAP1_TEXTURE_COORD_4 #x0D96)
		(define GL_MAP1_VERTEX_3 #x0D97)
		(define GL_MAP1_VERTEX_4 #x0D98)
		(define GL_MAP2_COLOR_4 #x0DB0)
		(define GL_MAP2_INDEX #x0DB1)
		(define GL_MAP2_NORMAL #x0DB2)
		(define GL_MAP2_TEXTURE_COORD_1 #x0DB3)
		(define GL_MAP2_TEXTURE_COORD_2 #x0DB4)
		(define GL_MAP2_TEXTURE_COORD_3 #x0DB5)
		(define GL_MAP2_TEXTURE_COORD_4 #x0DB6)
		(define GL_MAP2_VERTEX_3 #x0DB7)
		(define GL_MAP2_VERTEX_4 #x0DB8)
		(define GL_MAP1_GRID_DOMAIN #x0DD0)
		(define GL_MAP1_GRID_SEGMENTS #x0DD1)
		(define GL_MAP2_GRID_DOMAIN #x0DD2)
		(define GL_MAP2_GRID_SEGMENTS #x0DD3)
		(define GL_TEXTURE_1D #x0DE0)
		(define GL_TEXTURE_2D #x0DE1)
		(define GL_FEEDBACK_BUFFER_POINTER #x0DF0)
		(define GL_FEEDBACK_BUFFER_SIZE #x0DF1)
		(define GL_FEEDBACK_BUFFER_TYPE #x0DF2)
		(define GL_SELECTION_BUFFER_POINTER #x0DF3)
		(define GL_SELECTION_BUFFER_SIZE #x0DF4)
		(define GL_TEXTURE_WIDTH #x1000)
		(define GL_TEXTURE_HEIGHT #x1001)
		(define GL_TEXTURE_INTERNAL_FORMAT #x1003)
		(define GL_TEXTURE_BORDER_COLOR #x1004)
		(define GL_TEXTURE_BORDER #x1005)
		(define GL_DONT_CARE #x1100)
		(define GL_FASTEST #x1101)
		(define GL_NICEST #x1102)
		(define GL_LIGHT0 #x4000)
		(define GL_LIGHT1 #x4001)
		(define GL_LIGHT2 #x4002)
		(define GL_LIGHT3 #x4003)
		(define GL_LIGHT4 #x4004)
		(define GL_LIGHT5 #x4005)
		(define GL_LIGHT6 #x4006)
		(define GL_LIGHT7 #x4007)
		(define GL_AMBIENT #x1200)
		(define GL_DIFFUSE #x1201)
		(define GL_SPECULAR #x1202)
		(define GL_POSITION #x1203)
		(define GL_SPOT_DIRECTION #x1204)
		(define GL_SPOT_EXPONENT #x1205)
		(define GL_SPOT_CUTOFF #x1206)
		(define GL_CONSTANT_ATTENUATION #x1207)
		(define GL_LINEAR_ATTENUATION #x1208)
		(define GL_QUADRATIC_ATTENUATION #x1209)
		(define GL_COMPILE #x1300)
		(define GL_COMPILE_AND_EXECUTE #x1301)
		(define GL_CLEAR #x1500)
		(define GL_AND #x1501)
		(define GL_AND_REVERSE #x1502)
		(define GL_COPY #x1503)
		(define GL_AND_INVERTED #x1504)
		(define GL_NOOP #x1505)
		(define GL_XOR #x1506)
		(define GL_OR #x1507)
		(define GL_NOR #x1508)
		(define GL_EQUIV #x1509)
		(define GL_INVERT #x150A)
		(define GL_OR_REVERSE #x150B)
		(define GL_COPY_INVERTED #x150C)
		(define GL_OR_INVERTED #x150D)
		(define GL_NAND #x150E)
		(define GL_SET #x150F)
		(define GL_EMISSION #x1600)
		(define GL_SHININESS #x1601)
		(define GL_AMBIENT_AND_DIFFUSE #x1602)
		(define GL_COLOR_INDEXES #x1603)
		(define GL_MODELVIEW #x1700)
		(define GL_PROJECTION #x1701)
		(define GL_TEXTURE #x1702)
		(define GL_COLOR #x1800)
		(define GL_DEPTH #x1801)
		(define GL_STENCIL #x1802)
		(define GL_COLOR_INDEX #x1900)
		(define GL_STENCIL_INDEX #x1901)
		(define GL_DEPTH_COMPONENT #x1902)
		(define GL_RED #x1903)
		(define GL_GREEN #x1904)
		(define GL_BLUE #x1905)
		(define GL_ALPHA #x1906)
		(define GL_RGB #x1907)
		(define GL_RGBA #x1908)
		(define GL_LUMINANCE #x1909)
		(define GL_LUMINANCE_ALPHA #x190A)
		(define GL_BITMAP #x1A00)
		(define GL_POINT #x1B00)
		(define GL_LINE #x1B01)
		(define GL_FILL #x1B02)
		(define GL_RENDER #x1C00)
		(define GL_FEEDBACK #x1C01)
		(define GL_SELECT #x1C02)
		(define GL_FLAT #x1D00)
		(define GL_SMOOTH #x1D01)
		(define GL_KEEP #x1E00)
		(define GL_REPLACE #x1E01)
		(define GL_INCR #x1E02)
		(define GL_DECR #x1E03)
		(define GL_VENDOR #x1F00)
		(define GL_RENDERER #x1F01)
		(define GL_VERSION #x1F02)
		(define GL_EXTENSIONS #x1F03)
		(define GL_S #x2000)
		(define GL_T #x2001)
		(define GL_R #x2002)
		(define GL_Q #x2003)
		(define GL_MODULATE #x2100)
		(define GL_DECAL #x2101)
		(define GL_TEXTURE_ENV_MODE #x2200)
		(define GL_TEXTURE_ENV_COLOR #x2201)
		(define GL_TEXTURE_ENV #x2300)
		(define GL_EYE_LINEAR #x2400)
		(define GL_OBJECT_LINEAR #x2401)
		(define GL_SPHERE_MAP #x2402)
		(define GL_TEXTURE_GEN_MODE #x2500)
		(define GL_OBJECT_PLANE #x2501)
		(define GL_EYE_PLANE #x2502)
		(define GL_NEAREST #x2600)
		(define GL_LINEAR #x2601)
		(define GL_NEAREST_MIPMAP_NEAREST #x2700)
		(define GL_LINEAR_MIPMAP_NEAREST #x2701)
		(define GL_NEAREST_MIPMAP_LINEAR #x2702)
		(define GL_LINEAR_MIPMAP_LINEAR #x2703)
		(define GL_TEXTURE_MAG_FILTER #x2800)
		(define GL_TEXTURE_MIN_FILTER #x2801)
		(define GL_TEXTURE_WRAP_S #x2802)
		(define GL_TEXTURE_WRAP_T #x2803)
		(define GL_CLAMP #x2900)
		(define GL_REPEAT #x2901)


;WINGDIAPI void APIENTRY glAccum (GLenum op, GLfloat value);
;WINGDIAPI void APIENTRY glAlphaFunc (GLenum func, GLclampf ref);
  (define glBegin (dlsym % GLvoid "glBegin" GLenum))
;WINGDIAPI void APIENTRY glBitmap (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap);
  (define glBlendFunc (dlsym % GLvoid "glBlendFunc" GLenum GLenum))
;WINGDIAPI void APIENTRY glCallList (GLuint list);
;WINGDIAPI void APIENTRY glCallLists (GLsizei n, GLenum type, const GLvoid *lists);
  (define glClear (dlsym % GLvoid "glClear" GLbitfield))
;WINGDIAPI void APIENTRY glClearAccum (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
  (define glClearColor (dlsym % GLvoid "glClearColor" GLfloat GLfloat GLfloat GLfloat))
;WINGDIAPI void APIENTRY glClearIndex (GLfloat c);
;WINGDIAPI void APIENTRY glClipPlane (GLenum plane, const GLdouble *equation);
;WINGDIAPI void APIENTRY glColor3b (GLbyte red, GLbyte green, GLbyte blue);
;WINGDIAPI void APIENTRY glColor3bv (const GLbyte *v);
;WINGDIAPI void APIENTRY glColor3d (GLdouble red, GLdouble green, GLdouble blue);
;WINGDIAPI void APIENTRY glColor3dv (const GLdouble *v);
  (define glColor3f (dlsym % GLvoid "glColor3f" GLfloat GLfloat GLfloat))
;WINGDIAPI void APIENTRY glColor3fv (const GLfloat *v);
  (define glColor3i (dlsym % GLvoid "glColor3i" GLint GLint GLint))
;WINGDIAPI void APIENTRY glColor3iv (const GLint *v);
;WINGDIAPI void APIENTRY glColor3s (GLshort red, GLshort green, GLshort blue);
;WINGDIAPI void APIENTRY glColor3sv (const GLshort *v);
  (define glColor3ub (dlsym % GLvoid "glColor3ub" GLubyte GLubyte GLubyte))
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
;WINGDIAPI void APIENTRY glCopyPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type);
  (define glCullFace (dlsym % GLvoid "glCullFace" GLenum))
;WINGDIAPI void APIENTRY glDeleteLists (GLuint list, GLsizei range);
  (define glDisable (dlsym % GLvoid "glDisable" GLenum))
;WINGDIAPI void APIENTRY glDrawPixels (GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glEdgeFlag (GLboolean flag);
;WINGDIAPI void APIENTRY glEdgeFlagv (const GLboolean *flag);
  (define glEnable (dlsym % GLvoid "glEnable" GLenum))
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
  (define glFrontFace (dlsym % GLvoid "glFrontFace" GLenum))
;WINGDIAPI void APIENTRY glFrustum (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
;WINGDIAPI GLuint APIENTRY glGenLists (GLsizei range);
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
;WINGDIAPI void APIENTRY glGetPolygonStipple (GLubyte *mask);
  (define glGetString (dlsym % GLubyte* "glGetString" GLenum))
;WINGDIAPI void APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
;WINGDIAPI void APIENTRY glGetTexGendv (GLenum coord, GLenum pname, GLdouble *params);
;WINGDIAPI void APIENTRY glGetTexGenfv (GLenum coord, GLenum pname, GLfloat *params);
;WINGDIAPI void APIENTRY glGetTexGeniv (GLenum coord, GLenum pname, GLint *params);
  (define glHint (dlsym % GLvoid "glHint" GLenum GLenum))
;WINGDIAPI void APIENTRY glIndexMask (GLuint mask);
;WINGDIAPI void APIENTRY glIndexd (GLdouble c);
;WINGDIAPI void APIENTRY glIndexdv (const GLdouble *c);
;WINGDIAPI void APIENTRY glIndexf (GLfloat c);
;WINGDIAPI void APIENTRY glIndexfv (const GLfloat *c);
;WINGDIAPI void APIENTRY glIndexi (GLint c);
;WINGDIAPI void APIENTRY glIndexiv (const GLint *c);
;WINGDIAPI void APIENTRY glIndexs (GLshort c);
;WINGDIAPI void APIENTRY glIndexsv (const GLshort *c);
;WINGDIAPI void APIENTRY glInitNames (void);
;WINGDIAPI GLboolean APIENTRY glIsList (GLuint list);
;WINGDIAPI void APIENTRY glLightModelf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLightModeli (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightModeliv (GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
;WINGDIAPI void APIENTRY glLighti (GLenum light, GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glLightiv (GLenum light, GLenum pname, const GLint *params);
;WINGDIAPI void APIENTRY glLineStipple (GLint factor, GLushort pattern);
  (define glLineWidth (dlsym % GLvoid "glLineWidth" GLfloat))
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
;WINGDIAPI void APIENTRY glOrtho (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar);
  (define glOrtho (dlsym % GLvoid "glOrtho" GLdouble GLdouble  GLdouble GLdouble  GLdouble GLdouble))
;WINGDIAPI void APIENTRY glPassThrough (GLfloat token);
;WINGDIAPI void APIENTRY glPixelMapfv (GLenum map, GLsizei mapsize, const GLfloat *values);
;WINGDIAPI void APIENTRY glPixelMapuiv (GLenum map, GLsizei mapsize, const GLuint *values);
;WINGDIAPI void APIENTRY glPixelMapusv (GLenum map, GLsizei mapsize, const GLushort *values);
;WINGDIAPI void APIENTRY glPixelTransferf (GLenum pname, GLfloat param);
;WINGDIAPI void APIENTRY glPixelTransferi (GLenum pname, GLint param);
;WINGDIAPI void APIENTRY glPixelZoom (GLfloat xfactor, GLfloat yfactor);
   (define glPointSize (dlsym % GLvoid "glPointSize" GLfloat))
;WINGDIAPI void APIENTRY glPolygonStipple (const GLubyte *mask);
;WINGDIAPI void APIENTRY glPopAttrib (void);
;WINGDIAPI void APIENTRY glPopMatrix (void);
;WINGDIAPI void APIENTRY glPopName (void);
;WINGDIAPI void APIENTRY glPushAttrib (GLbitfield mask);
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
  (define glTexCoord2f (dlsym % GLvoid "glTexCoord2f" GLfloat GLfloat))
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
  (define glTexImage1D (dlsym % GLvoid "glTexImage1D" GLenum GLint GLint GLsizei GLint GLenum GLenum GLvoid*))
  (define glTexImage2D (dlsym % GLvoid "glTexImage2D" GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum GLvoid*))
  (define glTexParameteri (dlsym % GLvoid "glTexParameteri" GLenum GLenum GLint))

;WINGDIAPI void APIENTRY glTranslated (GLdouble x, GLdouble y, GLdouble z);
  (define glTranslatef (dlsym % GLvoid "glTranslatef" GLfloat GLfloat GLfloat))
  (define glVertex2d (dlsym % GLvoid "glVertex2d" GLdouble GLdouble))
;WINGDIAPI void APIENTRY glVertex2dv (const GLdouble *v);
  (define glVertex2f (dlsym % GLvoid "glVertex2f" GLfloat GLfloat))
;WINGDIAPI void APIENTRY glVertex2fv (const GLfloat *v);
  (define glVertex2i (dlsym % GLvoid "glVertex2i" GLint GLint))
;WINGDIAPI void APIENTRY glVertex2iv (const GLint *v);
;WINGDIAPI void APIENTRY glVertex2s (GLshort x, GLshort y);
;WINGDIAPI void APIENTRY glVertex2sv (const GLshort *v);
  (define glVertex3d (dlsym % GLvoid "glVertex3d" GLdouble GLdouble GLdouble))
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
  (define glViewport (dlsym % GLvoid "glViewport" GLint GLint GLsizei GLsizei))

))