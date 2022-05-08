; OpenGL 1.0 (1 Jul 1994)
; OpenGL base profile implementation
(define-library (OpenGL version-1-0)
(export

   GL_VERSION_1_0

   GL_TRUE GL_FALSE   ; 1, 0

   ;; 2.5 GL Errors
   glGetError ; GLenum ()

   ;; 2.6 Begin/End Paradigm
   ;; 2.6.1 Begin and End Objects
   glBegin ; void (GLenum mode)
   glEnd   ; void ()

   ;; 2.6.2 Polygon Edges
   glEdgeFlag  ; void (GLboolean flag)
   glEdgeFlagv ; void (const GLboolean *flag)

   ;; 2.7 Vertex Specification
   glVertex2d  ; void (GLdouble x, GLdouble y)
   glVertex2dv ; void (const GLdouble *v)
   glVertex2f  ; void (GLfloat x, GLfloat y)
   glVertex2fv ; void (const GLfloat *v)
   glVertex2i  ; void (GLint x, GLint y)
   glVertex2iv ; void (const GLint *v)
   glVertex2s  ; void (GLshort x, GLshort y)
   glVertex2sv ; void (const GLshort *v)
   glVertex3d  ; void (GLdouble x, GLdouble y, GLdouble z)
   glVertex3dv ; void (const GLdouble *v)
   glVertex3f  ; void (GLfloat x, GLfloat y, GLfloat z)
   glVertex3fv ; void (const GLfloat *v)
   glVertex3i  ; void (GLint x, GLint y, GLint z)
   glVertex3iv ; void (const GLint *v)
   glVertex3s  ; void (GLshort x, GLshort y, GLshort z)
   glVertex3sv ; void (const GLshort *v)
   glVertex4d  ; void (GLdouble x, GLdouble y, GLdouble z, GLdouble w)
   glVertex4dv ; void (const GLdouble *v)
   glVertex4f  ; void (GLfloat x, GLfloat y, GLfloat z, GLfloat w)
   glVertex4fv ; void (const GLfloat *v)
   glVertex4i  ; void (GLint x, GLint y, GLint z, GLint w)
   glVertex4iv ; void (const GLint *v)
   glVertex4s  ; void (GLshort x, GLshort y, GLshort z, GLshort w)
   glVertex4sv ; void (const GLshort *v)

   glTexCoord1d  ; void (GLdouble s)
   glTexCoord1dv ; void (const GLdouble *v)
   glTexCoord1f  ; void (GLfloat s)
   glTexCoord1fv ; void (const GLfloat *v)
   glTexCoord1i  ; void (GLint s)
   glTexCoord1iv ; void (const GLint *v)
   glTexCoord1s  ; void (GLshort s)
   glTexCoord1sv ; void (const GLshort *v)
   glTexCoord2d  ; void (GLdouble s, GLdouble t)
   glTexCoord2dv ; void (const GLdouble *v)
   glTexCoord2f  ; void (GLfloat GLfloat)
   glTexCoord2fv ; void (const GLfloat *v)
   glTexCoord2i  ; void (GLint s, GLint t)
   glTexCoord2iv ; void (const GLint *v)
   glTexCoord2s  ; void (GLshort s, GLshort t)
   glTexCoord2sv ; void (const GLshort *v)
   glTexCoord3d  ; void (GLdouble s, GLdouble t, GLdouble r)
   glTexCoord3dv ; void (const GLdouble *v)
   glTexCoord3f  ; void (GLfloat s, GLfloat t, GLfloat r)
   glTexCoord3fv ; void (const GLfloat *v)
   glTexCoord3i  ; void (GLint s, GLint t, GLint r)
   glTexCoord3iv ; void (const GLint *v)
   glTexCoord3s  ; void (GLshort s, GLshort t, GLshort r)
   glTexCoord3sv ; void (const GLshort *v)
   glTexCoord4d  ; void (GLdouble s, GLdouble t, GLdouble r, GLdouble q)
   glTexCoord4dv ; void (const GLdouble *v)
   glTexCoord4f  ; void (GLfloat s, GLfloat t, GLfloat r, GLfloat q)
   glTexCoord4fv ; void (const GLfloat *v)
   glTexCoord4i  ; void (GLint s, GLint t, GLint r, GLint q)
   glTexCoord4iv ; void (const GLint *v)
   glTexCoord4s  ; void (GLshort s, GLshort t, GLshort r, GLshort q)
   glTexCoord4sv ; void (const GLshort *v)

   glNormal3b  ; void (GLbyte nx, GLbyte ny, GLbyte nz)
   glNormal3bv ; void (const GLbyte *v)
   glNormal3d  ; void (GLdouble nx, GLdouble ny, GLdouble nz)
   glNormal3dv ; void (const GLdouble *v)
   glNormal3f  ; void (GLfloat nx, GLfloat ny, GLfloat nz)
   glNormal3fv ; void (const GLfloat *v)
   glNormal3i  ; void (GLint nx, GLint ny, GLint nz)
   glNormal3iv ; void (const GLint *v)
   glNormal3s  ; void (GLshort nx, GLshort ny, GLshort nz)
   glNormal3sv ; void (const GLshort *v)

   glColor3b   ; void (GLbyte red, GLbyte green, GLbyte blue)
   glColor3bv  ; void (const GLbyte *v)
   glColor3d   ; void (GLdouble red, GLdouble green, GLdouble blue)
   glColor3dv  ; void (const GLdouble *v)
   glColor3f   ; void (GLfloat red, GLfloat green, GLfloat blue)
   glColor3fv  ; void (const GLfloat *v)
   glColor3i   ; void (GLint red, GLint green, GLint blue)
   glColor3iv  ; void (const GLint *v)
   glColor3s   ; void (GLshort red, GLshort green, GLshort blue)
   glColor3sv  ; void (const GLshort *v)
   glColor3ub  ; void (GLubyte red, GLubyte green, GLubyte blue)
   glColor3ubv ; void (const GLubyte *v)
   glColor3ui  ; void (GLuint red, GLuint green, GLuint blue)
   glColor3uiv ; void (const GLuint *v)
   glColor3us  ; void (GLushort red, GLushort green, GLushort blue)
   glColor3usv ; void (const GLushort *v)
   glColor4b   ; void (GLbyte red, GLbyte green, GLbyte blue, GLbyte alpha)
   glColor4bv  ; void (const GLbyte *v)
   glColor4d   ; void (GLdouble red, GLdouble green, GLdouble blue, GLdouble alpha)
   glColor4dv  ; void (const GLdouble *v)
   glColor4f   ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
   glColor4fv  ; void (const GLfloat *v)
   glColor4i   ; void (GLint red, GLint green, GLint blue, GLint alpha)
   glColor4iv  ; void (const GLint *v)
   glColor4s   ; void (GLshort red, GLshort green, GLshort blue, GLshort alpha)
   glColor4sv  ; void (const GLshort *v)
   glColor4ub  ; void (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha)
   glColor4ubv ; void (const GLubyte *v)
   glColor4ui  ; void (GLuint red, GLuint green, GLuint blue, GLuint alpha)
   glColor4uiv ; void (const GLuint *v)
   glColor4us  ; void (GLushort red, GLushort green, GLushort blue, GLushort alpha)
   glColor4usv ; void (const GLushort *v)

   glIndexd  ; void (GLdouble c)
   glIndexdv ; void (const GLdouble *c)
   glIndexf  ; void (GLfloat c)
   glIndexfv ; void (const GLfloat *c)
   glIndexi  ; void (GLint c)
   glIndexiv ; void (const GLint *c)
   glIndexs  ; void (GLshort c)
   glIndexsv ; void (const GLshort *c)

   ;; 2.8 Rectangles
   glRectd  ; void (GLdouble x1, GLdouble y1, GLdouble x2, GLdouble y2)
   glRectdv ; void (const GLdouble *v1, const GLdouble *v2)
   glRectf  ; void (GLfloat x1, GLfloat y1, GLfloat x2, GLfloat y2)
   glRectfv ; void (const GLfloat *v1, const GLfloat *v2)
   glRecti  ; void (GLint x1, GLint y1, GLint x2, GLint y2)
   glRectiv ; void (const GLint *v1, const GLint *v2)
   glRects  ; void (GLshort x1, GLshort y1, GLshort x2, GLshort y2)
   glRectsv ; void (const GLshort *v1, const GLshort *v2)

   ;; 2.9 Coordinate Transformations
   ;; 2.9.1 Controlling the Viewport
   glDepthRange ; void (GLclampd zNear, GLclampd zFar)
   glViewport ; void (GLint x, GLint y, GLsizei width, GLsizei height)

   ;; 2.9.2 Matrices
   glMatrixMode  ; void (GLenum mode)
   glLoadMatrixd ; void (const GLdouble *m)
   glLoadMatrixf ; void (const GLfloat *m)
   glMultMatrixd ; void (const GLdouble *m)
   glMultMatrixf ; void (const GLfloat *m)
   glLoadIdentity; void (void)

   glRotated    ; void (GLdouble angle, GLdouble x, GLdouble y, GLdouble z)
   glRotatef    ; void (GLfloat angle, GLfloat x, GLfloat y, GLfloat z)
   glTranslated ; void (GLdouble x, GLdouble y, GLdouble z)
   glTranslatef ; void (GLfloat x, GLfloat y, GLfloat z)
   glScaled     ; void (GLdouble x, GLdouble y, GLdouble z)
   glScalef     ; void (GLfloat x, GLfloat y, GLfloat z)

   glFrustum    ; void (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)
   glOrtho      ; void (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top, GLdouble zNear, GLdouble zFar)

   glPushMatrix ; void glPushMatrix (void)
   glPopMatrix  ; void glPopMatrix (void)

   ;; 2.9.3 Normal Transformation
   glEnable  ; void (GLenum cap)
   glDisable ; void (GLenum cap)

   ;; 2.9.4 Generating texture coordinates
   glTexGend  ; void glTexGend (GLenum coord, GLenum pname, GLdouble param)
   glTexGendv ; void glTexGendv (GLenum coord, GLenum pname, const GLdouble *params)
   glTexGenf  ; void glTexGenf (GLenum coord, GLenum pname, GLfloat param)
   glTexGenfv ; void glTexGenfv (GLenum coord, GLenum pname, const GLfloat *params)
   glTexGeni  ; void glTexGeni (GLenum coord, GLenum pname, GLint param)
   glTexGeniv ; void glTexGeniv (GLenum coord, GLenum pname, const GLint *params)

   ;; 2.10 Clipping
   glClipPlane ; void (GLenum plane, const GLdouble *equation)

   ;; 2.11 Current Ruster Position
   glRasterPos2d  ; void (GLdouble x, GLdouble y)
   glRasterPos2dv ; void (const GLdouble *v)
   glRasterPos2f  ; void (GLfloat x, GLfloat y)
   glRasterPos2fv ; void (const GLfloat *v)
   glRasterPos2i  ; void (GLint x, GLint y)
   glRasterPos2iv ; void (const GLint *v)
   glRasterPos2s  ; void (GLshort x, GLshort y)
   glRasterPos2sv ; void (const GLshort *v)
   glRasterPos3d  ; void (GLdouble x, GLdouble y, GLdouble z)
   glRasterPos3dv ; void (const GLdouble *v)
   glRasterPos3f  ; void (GLfloat x, GLfloat y, GLfloat z)
   glRasterPos3fv ; void (const GLfloat *v)
   glRasterPos3i  ; void (GLint x, GLint y, GLint z)
   glRasterPos3iv ; void (const GLint *v)
   glRasterPos3s  ; void (GLshort x, GLshort y, GLshort z)
   glRasterPos3sv ; void (const GLshort *v)
   glRasterPos4d  ; void (GLdouble x, GLdouble y, GLdouble z, GLdouble w)
   glRasterPos4dv ; void (const GLdouble *v)
   glRasterPos4f  ; void (GLfloat x, GLfloat y, GLfloat z, GLfloat w)
   glRasterPos4fv ; void (const GLfloat *v)
   glRasterPos4i  ; void (GLint x, GLint y, GLint z, GLint w)
   glRasterPos4iv ; void (const GLint *v)
   glRasterPos4s  ; void (GLshort x, GLshort y, GLshort z, GLshort w)
   glRasterPos4sv ; void (const GLshort *v)

   ;; 2.12 Colors and Coloring
   ;; 2.12.1 Lighting
   glFrontFace  ; void (GLenum direction)

   ;; 2.12.2 Lighting Parameter Specication
   glMaterialf  ; void (GLenum face, GLenum pname, GLfloat param)
   glMaterialfv ; void (GLenum face, GLenum pname, const GLfloat *params)
   glMateriali  ; void (GLenum face, GLenum pname, GLint param)
   glMaterialiv ; void (GLenum face, GLenum pname, const GLint *params)

   glLightf  ; void (GLenum light, GLenum pname, GLfloat param)
   glLightfv ; void (GLenum light, GLenum pname, const GLfloat *params)
   glLighti  ; void (GLenum light, GLenum pname, GLint param)
   glLightiv ; void (GLenum light, GLenum pname, const GLint *params)

   glLightModelf  ; void (GLenum pname, GLfloat param)
   glLightModelfv ; void (GLenum pname, const GLfloat *params)
   glLightModeli  ; void (GLenum pname, GLint param)
   glLightModeliv ; void (GLenum pname, const GLint *params)

   ;; 2.12.3 ColorMaterial
   glColorMaterial ; void (GLenum face, GLenum mode)

   ;; 2.12.7 Flatshading
   glShadeModel ; void (GLenum model)

   ;; 3.3 Points
   glPointSize ; void (GLfloat size)

   ;; 3.4 Line Segments
   glLineWidth ; void (GLfloat width)
   glLineStipple ; void (GLint factor, GLushort pattern)

   ;; 3.5 Polygons
   glCullFace ; void (GLenum mode)
   glPolygonStipple ; void glPolygonStipple (const GLubyte *mask)
   glPolygonMode ; void (GLenum face, GLenum mode)

   ;; 3.6. Pixel Rectangles
   glPixelStoref ; void (GLenum pname, GLfloat param)
   glPixelStorei ; void (GLenum pname, GLint param)
   glPixelTransferf ; void (GLenum pname, GLfloat param)
   glPixelTransferi ; void (GLenum pname, GLint param)

   glPixelMapfv  ; void (GLenum map, GLsizei mapsize, const GLfloat *values)
   glPixelMapuiv ; void (GLenum map, GLsizei mapsize, const GLuint *values)
   glPixelMapusv ; void (GLenum map, GLsizei mapsize, const GLushort *values)

   ;; 3.6.3 Rasterization of Pixel Rectangles
   glDrawPixels ; void (GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels)
   glPixelZoom  ; void (GLfloat xfactor, GLfloat yfactor)

   ;; 3.7. Bitmaps
   glBitmap ; void (GLsizei width, GLsizei height, GLfloat xorig, GLfloat yorig, GLfloat xmove, GLfloat ymove, const GLubyte *bitmap)

   ;; 3.8. Texturing
   glTexImage2D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const GLvoid *pixels)
   glTexImage1D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLint border, GLenum format, GLenum type, const GLvoid *pixels)

   glTexParameterf  ; void (GLenum target, GLenum pname, GLfloat param)
   glTexParameterfv ; void (GLenum target, GLenum pname, const GLfloat *params)
   glTexParameteri  ; void (GLenum target, GLenum pname, GLint param)
   glTexParameteriv ; void (GLenum target, GLenum pname, const GLint *params)

   ;; 3.8.3 Texture Environments and Texture Functions
   glTexEnvf  ; void (GLenum target, GLenum pname, GLfloat param)
   glTexEnvfv ; void (GLenum target, GLenum pname, const GLfloat *params)
   glTexEnvi  ; void (GLenum target, GLenum pname, GLint param)
   glTexEnviv ; void (GLenum target, GLenum pname, const GLint *params)

   ;; 3.9. Fog
   glFogf  ; void (GLenum pname, GLfloat param)
   glFogfv ; void (GLenum pname, const GLfloat *params)
   glFogi  ; void (GLenum pname, GLint param)
   glFogiv ; void (GLenum pname, const GLint *params)

   ;; 4.1 Per-Fragment Operations
   glScissor   ; void glScissor (GLint x, GLint y, GLsizei width, GLsizei height)
   glAlphaFunc ; void (GLenum func, GLclampf ref)
   glStencilFunc ;void (GLenum func, GLint ref, GLuint mask)
   glStencilOp ; void (GLenum fail, GLenum zfail, GLenum zpass)
   glDepthFunc ; void (GLenum func)
   glBlendFunc ; void (GLenum sfactor, GLenum dfactor)
   glLogicOp   ; void (GLenum opcode)

   ;; 4.2 Whole Framebuffer Operations
   glDrawBuffer ; void (GLenum mode)

   ;; 4.2.2 Fine Control of Buffer Updates
   glIndexMask   ; void (GLuint mask)
   glColorMask   ; void (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)
   glDepthMask   ; void (GLboolean flag)
   glStencilMask ; void (GLuint mask)

   ;; 4.2.3 Clearing the Buffers
   glClear ; void (GLbitfield mask)
   glClearColor ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
   glClearIndex ; void (GLfloat c)
   glClearDepth ; void (GLclampd depth)
   glClearStencil ; void (GLint s)
   glClearAccum ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)

   ;; 4.2.4 The Accumulation Buffer
   glAccum ; void (GLenum op, GLfloat value)

   ;; 4.3 Drawing, Reading, and Copying Pixels
   glReadPixels ; void (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, GLvoid *pixels)
   glReadBuffer ; void (GLenum mode)
   glCopyPixels ; void (GLint x, GLint y, GLsizei width, GLsizei height, GLenum type)

   ;; 5.1 Evaluators
   glMap1d ; void (GLenum target, GLdouble u1, GLdouble u2, GLint stride, GLint order, const GLdouble *points)
   glMap1f ; void (GLenum target, GLfloat u1, GLfloat u2, GLint stride, GLint order, const GLfloat *points)
   glMap2d ; void (GLenum target, GLdouble u1, GLdouble u2, GLint ustride, GLint uorder, GLdouble v1, GLdouble v2, GLint vstride, GLint vorder, const GLdouble *points)
   glMap2f ; void (GLenum target, GLfloat u1, GLfloat u2, GLint ustride, GLint uorder, GLfloat v1, GLfloat v2, GLint vstride, GLint vorder, const GLfloat *points)
   glEvalCoord1d  ; void (GLdouble u)
   glEvalCoord1dv ; void (const GLdouble *u)
   glEvalCoord1f  ; void (GLfloat u)
   glEvalCoord1fv ; void (const GLfloat *u)
   glEvalCoord2d  ; void (GLdouble u, GLdouble v)
   glEvalCoord2dv ; void (const GLdouble *u)
   glEvalCoord2f  ; void (GLfloat u, GLfloat v)
   glEvalCoord2fv ; void (const GLfloat *u)
   glMapGrid1d ; void (GLint un, GLdouble u1, GLdouble u2)
   glMapGrid1f ; void (GLint un, GLfloat u1, GLfloat u2)
   glMapGrid2d ; void (GLint un, GLdouble u1, GLdouble u2, GLint vn, GLdouble v1, GLdouble v2)
   glMapGrid2f ; void (GLint un, GLfloat u1, GLfloat u2, GLint vn, GLfloat v1, GLfloat v2)
   glEvalMesh1 ; void (GLenum mode, GLint i1, GLint i2)
   glEvalMesh2 ; void (GLenum mode, GLint i1, GLint i2, GLint j1, GLint j2)
   glEvalPoint1 ; void (GLint i)
   glEvalPoint2 ; void (GLint i, GLint j)

   ;; 5.2. Selection
   glInitNames    ; void glInitNames (void)
   glPopName      ; void glPopName (void)
   glPushName     ; void glPushName (GLuint name)
   glLoadName     ; void glLoadName (GLuint name)
   glRenderMode   ; GLint glRenderMode (GLenum mode)
   glSelectBuffer ; void glSelectBuffer (GLsizei size, GLuint *buffer)

   ;; 5.3. Feedback
   glFeedbackBuffer ; void (GLsizei size, GLenum type, GLfloat *buffer)
   glPassThrough ; void (GLfloat token)

   ;; 5.4 Display Lists
   glNewList   ; void (GLuint list, GLenum mode)
   glEndList   ; void glEndList (void)
   glCallList  ; void (GLuint list)
   glCallLists ; void (GLsizei n, GLenum type, const GLvoid *lists)
   glListBase  ; void (GLuint base)
   glGenLists  ; GLuint (GLsizei range)
   glIsList    ; GLboolean (GLuint list)
   glDeleteLists ; void (GLuint list, GLsizei range)

   ;; 5.5 Flush and Finish
   glFlush  ; void (void)
   glFinish ; void (void)

   ;; 5.6 Hints
   glHint ; void (GLenum target, GLenum mode)

   ;; 6 State and State Requests
   glGetBooleanv ; void (GLenum pname, GLboolean *params)
   glGetIntegerv ; void (GLenum pname, GLint *params)
   glGetFloatv   ; void (GLenum pname, GLfloat *params)
   glGetDoublev  ; void (GLenum pname, GLdouble *params)

   glIsEnabled ; GLboolean (GLenum cap)

   glGetClipPlane ; void (GLenum plane, GLdouble *equation)
   glGetLightfv ; void (GLenum light, GLenum pname, GLfloat *params)
   glGetLightiv ; void (GLenum light, GLenum pname, GLint *params)
   glGetMaterialfv ; void (GLenum face, GLenum pname, GLfloat *params)
   glGetMaterialiv ; void (GLenum face, GLenum pname, GLint *params)
   glGetTexEnvfv ; void (GLenum target, GLenum pname, GLfloat *params)
   glGetTexEnviv ; void (GLenum target, GLenum pname, GLint *params)
   glGetTexGenfv ; void (GLenum coord, GLenum pname, GLfloat *params)
   glGetTexGeniv ; void (GLenum coord, GLenum pname, GLint *params)
   glGetTexParameterfv ; void (GLenum target, GLenum pname, GLfloat *params)
   glGetTexParameteriv ; void (GLenum target, GLenum pname, GLint *params)
   glGetTexLevelParameterfv ; void (GLenum target, GLint level, GLenum pname, GLfloat *params)
   glGetTexLevelParameteriv ; void (GLenum target, GLint level, GLenum pname, GLint *params)
   glGetPixelMapfv  ; void (GLenum map, GLfloat *values)
   glGetPixelMapuiv ; void (GLenum map, GLuint *values)
   glGetPixelMapusv ; void (GLenum map, GLushort *values)
   glGetMapdv ; void (GLenum target, GLenum query, GLdouble *v)
   glGetMapfv ; void (GLenum target, GLenum query, GLfloat *v)
   glGetMapiv ; void (GLenum target, GLenum query, GLint *v)
   glGetTexImage ; void (GLenum target, GLint level, GLenum format, GLenum type, GLvoid *pixels)
   glGetPolygonStipple ; void (GLubyte *mask)

   glGetString  ; GLubyte* (GLenum name)

   glPushAttrib ; void (GLbitfield mask)
   glPopAttrib  ; void (void)

   ; -------------------------------------------------------------------------
   ; AccumOp
   GL_ACCUM
   GL_LOAD
   GL_RETURN
   GL_MULT
   GL_ADD

   ; AlphaFunction
   GL_NEVER
   GL_LESS
   GL_EQUAL
   GL_LEQUAL
   GL_GREATER
   GL_NOTEQUAL
   GL_GEQUAL
   GL_ALWAYS

   ; AttribMask
   GL_CURRENT_BIT
   GL_POINT_BIT
   GL_LINE_BIT
   GL_POLYGON_BIT
   GL_POLYGON_STIPPLE_BIT
   GL_PIXEL_MODE_BIT
   GL_LIGHTING_BIT
   GL_FOG_BIT
   GL_DEPTH_BUFFER_BIT
   GL_ACCUM_BUFFER_BIT
   GL_STENCIL_BUFFER_BIT
   GL_VIEWPORT_BIT
   GL_TRANSFORM_BIT
   GL_ENABLE_BIT
   GL_COLOR_BUFFER_BIT
   GL_HINT_BIT
   GL_EVAL_BIT
   GL_LIST_BIT
   GL_TEXTURE_BIT
   GL_SCISSOR_BIT
   GL_ALL_ATTRIB_BITS

   ; BeginMode
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

   ; BlendingFactorDest
   GL_ZERO
   GL_ONE
   GL_SRC_COLOR
   GL_ONE_MINUS_SRC_COLOR
   GL_SRC_ALPHA
   GL_ONE_MINUS_SRC_ALPHA
   GL_DST_ALPHA
   GL_ONE_MINUS_DST_ALPHA

   ; BlendingFactorSrc
   ; GL_ZERO
   ; GL_ONE
   GL_DST_COLOR
   GL_ONE_MINUS_DST_COLOR
   GL_SRC_ALPHA_SATURATE
   ; GL_SRC_ALPHA
   ; GL_ONE_MINUS_SRC_ALPHA
   ; GL_DST_ALPHA
   ; GL_ONE_MINUS_DST_ALPHA

   ; ClearBufferMask
   ; GL_COLOR_BUFFER_BIT
   ; GL_ACCUM_BUFFER_BIT
   ; GL_STENCIL_BUFFER_BIT
   ; GL_DEPTH_BUFFER_BIT

   ; ClipPlaneName
   GL_CLIP_PLANE0
   GL_CLIP_PLANE1
   GL_CLIP_PLANE2
   GL_CLIP_PLANE3
   GL_CLIP_PLANE4
   GL_CLIP_PLANE5

   ; ColorMaterialFace
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; ColorMaterialParameter
   ; GL_AMBIENT
   ; GL_DIFFUSE
   ; GL_SPECULAR
   ; GL_EMISSION
   ; GL_AMBIENT_AND_DIFFUSE

   ; ColorPointerType
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT

   ; CullFaceMode
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; DataType
   GL_BYTE
   GL_UNSIGNED_BYTE
   GL_SHORT
   GL_UNSIGNED_SHORT
   GL_INT
   GL_UNSIGNED_INT
   GL_FLOAT
   GL_2_BYTES
   GL_3_BYTES
   GL_4_BYTES

   ; DepthFunction
   ; GL_NEVER
   ; GL_LESS
   ; GL_EQUAL
   ; GL_LEQUAL
   ; GL_GREATER
   ; GL_NOTEQUAL
   ; GL_GEQUAL
   ; GL_ALWAYS

   ; DrawBufferMode
   GL_NONE
   GL_FRONT_LEFT
   GL_FRONT_RIGHT
   GL_BACK_LEFT
   GL_BACK_RIGHT
   GL_FRONT
   GL_BACK
   GL_LEFT
   GL_RIGHT
   GL_FRONT_AND_BACK
   GL_AUX0
   GL_AUX1
   GL_AUX2
   GL_AUX3

   ; Enable
   ; GL_FOG
   ; GL_LIGHTING
   ; GL_TEXTURE_1D
   ; GL_TEXTURE_2D
   ; GL_LINE_STIPPLE
   ; GL_POLYGON_STIPPLE
   ; GL_CULL_FACE
   ; GL_ALPHA_TEST
   ; GL_BLEND
   ; GL_LOGIC_OP
   ; GL_COLOR_LOGIC_OP
   ; GL_DITHER
   ; GL_STENCIL_TEST
   ; GL_DEPTH_TEST
   ; GL_CLIP_PLANE0
   ; GL_CLIP_PLANE1
   ; GL_CLIP_PLANE2
   ; GL_CLIP_PLANE3
   ; GL_CLIP_PLANE4
   ; GL_CLIP_PLANE5
   ; GL_LIGHT0
   ; GL_LIGHT1
   ; GL_LIGHT2
   ; GL_LIGHT3
   ; GL_LIGHT4
   ; GL_LIGHT5
   ; GL_LIGHT6
   ; GL_LIGHT7
   ; GL_TEXTURE_GEN_S
   ; GL_TEXTURE_GEN_T
   ; GL_TEXTURE_GEN_R
   ; GL_TEXTURE_GEN_Q
   ; GL_MAP1_VERTEX_3
   ; GL_MAP1_VERTEX_4
   ; GL_MAP1_COLOR_4
   ; GL_MAP1_INDEX
   ; GL_MAP1_NORMAL
   ; GL_MAP1_TEXTURE_COORD_1
   ; GL_MAP1_TEXTURE_COORD_2
   ; GL_MAP1_TEXTURE_COORD_3
   ; GL_MAP1_TEXTURE_COORD_4
   ; GL_MAP2_VERTEX_3
   ; GL_MAP2_VERTEX_4
   ; GL_MAP2_COLOR_4
   ; GL_MAP2_INDEX
   ; GL_MAP2_NORMAL
   ; GL_MAP2_TEXTURE_COORD_1
   ; GL_MAP2_TEXTURE_COORD_2
   ; GL_MAP2_TEXTURE_COORD_3
   ; GL_MAP2_TEXTURE_COORD_4
   ; GL_POINT_SMOOTH
   ; GL_LINE_SMOOTH
   ; GL_POLYGON_SMOOTH
   ; GL_SCISSOR_TEST
   ; GL_COLOR_MATERIAL
   ; GL_NORMALIZE
   ; GL_AUTO_NORMAL

   ; ErrorCode
   GL_NO_ERROR
   GL_INVALID_ENUM
   GL_INVALID_VALUE
   GL_INVALID_OPERATION
   GL_STACK_OVERFLOW
   GL_STACK_UNDERFLOW
   GL_OUT_OF_MEMORY

   ; FeedBackMode
   GL_2D
   GL_3D
   GL_3D_COLOR
   GL_3D_COLOR_TEXTURE
   GL_4D_COLOR_TEXTURE

   ; FeedBackToken
   GL_PASS_THROUGH_TOKEN
   GL_POINT_TOKEN
   GL_LINE_TOKEN
   GL_POLYGON_TOKEN
   GL_BITMAP_TOKEN
   GL_DRAW_PIXEL_TOKEN
   GL_COPY_PIXEL_TOKEN
   GL_LINE_RESET_TOKEN

   ; FogMode
   ; GL_LINEAR
   GL_EXP
   GL_EXP2

   ; FogParameter
   ; GL_FOG_COLOR
   ; GL_FOG_DENSITY
   ; GL_FOG_END
   ; GL_FOG_INDEX
   ; GL_FOG_MODE
   ; GL_FOG_START

   ; FrontFaceDirection
   GL_CW
   GL_CCW

   ; GetMapTarget
   GL_COEFF
   GL_ORDER
   GL_DOMAIN

   ; GetPixelMap
   ; GL_PIXEL_MAP_I_TO_I
   ; GL_PIXEL_MAP_S_TO_S
   ; GL_PIXEL_MAP_I_TO_R
   ; GL_PIXEL_MAP_I_TO_G
   ; GL_PIXEL_MAP_I_TO_B
   ; GL_PIXEL_MAP_I_TO_A
   ; GL_PIXEL_MAP_R_TO_R
   ; GL_PIXEL_MAP_G_TO_G
   ; GL_PIXEL_MAP_B_TO_B
   ; GL_PIXEL_MAP_A_TO_A

   ; GetTarget
   GL_CURRENT_COLOR
   GL_CURRENT_INDEX
   GL_CURRENT_NORMAL
   GL_CURRENT_TEXTURE_COORDS
   GL_CURRENT_RASTER_COLOR
   GL_CURRENT_RASTER_INDEX
   GL_CURRENT_RASTER_TEXTURE_COORDS
   GL_CURRENT_RASTER_POSITION
   GL_CURRENT_RASTER_POSITION_VALID
   GL_CURRENT_RASTER_DISTANCE
   GL_POINT_SMOOTH
   GL_POINT_SIZE
   GL_POINT_SIZE_RANGE
   GL_POINT_SIZE_GRANULARITY
   GL_LINE_SMOOTH
   GL_LINE_WIDTH
   GL_LINE_WIDTH_RANGE
   GL_LINE_WIDTH_GRANULARITY
   GL_LINE_STIPPLE
   GL_LINE_STIPPLE_PATTERN
   GL_LINE_STIPPLE_REPEAT
   GL_LIST_MODE
   GL_MAX_LIST_NESTING
   GL_LIST_BASE
   GL_LIST_INDEX
   GL_POLYGON_MODE
   GL_POLYGON_SMOOTH
   GL_POLYGON_STIPPLE
   GL_EDGE_FLAG
   GL_CULL_FACE
   GL_CULL_FACE_MODE
   GL_FRONT_FACE
   GL_LIGHTING
   GL_LIGHT_MODEL_LOCAL_VIEWER
   GL_LIGHT_MODEL_TWO_SIDE
   GL_LIGHT_MODEL_AMBIENT
   GL_SHADE_MODEL
   GL_COLOR_MATERIAL_FACE
   GL_COLOR_MATERIAL_PARAMETER
   GL_COLOR_MATERIAL
   GL_FOG
   GL_FOG_INDEX
   GL_FOG_DENSITY
   GL_FOG_START
   GL_FOG_END
   GL_FOG_MODE
   GL_FOG_COLOR
   GL_DEPTH_RANGE
   GL_DEPTH_TEST
   GL_DEPTH_WRITEMASK
   GL_DEPTH_CLEAR_VALUE
   GL_DEPTH_FUNC
   GL_ACCUM_CLEAR_VALUE
   GL_STENCIL_TEST
   GL_STENCIL_CLEAR_VALUE
   GL_STENCIL_FUNC
   GL_STENCIL_VALUE_MASK
   GL_STENCIL_FAIL
   GL_STENCIL_PASS_DEPTH_FAIL
   GL_STENCIL_PASS_DEPTH_PASS
   GL_STENCIL_REF
   GL_STENCIL_WRITEMASK
   GL_MATRIX_MODE
   GL_NORMALIZE
   GL_VIEWPORT
   GL_MODELVIEW_STACK_DEPTH
   GL_PROJECTION_STACK_DEPTH
   GL_TEXTURE_STACK_DEPTH
   GL_MODELVIEW_MATRIX
   GL_PROJECTION_MATRIX
   GL_TEXTURE_MATRIX
   GL_ATTRIB_STACK_DEPTH
   GL_ALPHA_TEST
   GL_ALPHA_TEST_FUNC
   GL_ALPHA_TEST_REF
   GL_DITHER
   GL_BLEND_DST
   GL_BLEND_SRC
   GL_BLEND
   GL_LOGIC_OP_MODE
   GL_LOGIC_OP
   GL_COLOR_LOGIC_OP
   GL_AUX_BUFFERS
   GL_DRAW_BUFFER
   GL_READ_BUFFER
   GL_SCISSOR_BOX
   GL_SCISSOR_TEST
   GL_INDEX_CLEAR_VALUE
   GL_INDEX_WRITEMASK
   GL_COLOR_CLEAR_VALUE
   GL_COLOR_WRITEMASK
   GL_INDEX_MODE
   GL_RGBA_MODE
   GL_DOUBLEBUFFER
   GL_STEREO
   GL_RENDER_MODE
   GL_PERSPECTIVE_CORRECTION_HINT
   GL_POINT_SMOOTH_HINT
   GL_LINE_SMOOTH_HINT
   GL_POLYGON_SMOOTH_HINT
   GL_FOG_HINT
   GL_TEXTURE_GEN_S
   GL_TEXTURE_GEN_T
   GL_TEXTURE_GEN_R
   GL_TEXTURE_GEN_Q
   GL_PIXEL_MAP_I_TO_I
   GL_PIXEL_MAP_S_TO_S
   GL_PIXEL_MAP_I_TO_R
   GL_PIXEL_MAP_I_TO_G
   GL_PIXEL_MAP_I_TO_B
   GL_PIXEL_MAP_I_TO_A
   GL_PIXEL_MAP_R_TO_R
   GL_PIXEL_MAP_G_TO_G
   GL_PIXEL_MAP_B_TO_B
   GL_PIXEL_MAP_A_TO_A
   GL_PIXEL_MAP_I_TO_I_SIZE
   GL_PIXEL_MAP_S_TO_S_SIZE
   GL_PIXEL_MAP_I_TO_R_SIZE
   GL_PIXEL_MAP_I_TO_G_SIZE
   GL_PIXEL_MAP_I_TO_B_SIZE
   GL_PIXEL_MAP_I_TO_A_SIZE
   GL_PIXEL_MAP_R_TO_R_SIZE
   GL_PIXEL_MAP_G_TO_G_SIZE
   GL_PIXEL_MAP_B_TO_B_SIZE
   GL_PIXEL_MAP_A_TO_A_SIZE
   GL_UNPACK_SWAP_BYTES
   GL_UNPACK_LSB_FIRST
   GL_UNPACK_ROW_LENGTH
   GL_UNPACK_SKIP_ROWS
   GL_UNPACK_SKIP_PIXELS
   GL_UNPACK_ALIGNMENT
   GL_PACK_SWAP_BYTES
   GL_PACK_LSB_FIRST
   GL_PACK_ROW_LENGTH
   GL_PACK_SKIP_ROWS
   GL_PACK_SKIP_PIXELS
   GL_PACK_ALIGNMENT
   GL_MAP_COLOR
   GL_MAP_STENCIL
   GL_INDEX_SHIFT
   GL_INDEX_OFFSET
   GL_RED_SCALE
   GL_RED_BIAS
   GL_ZOOM_X
   GL_ZOOM_Y
   GL_GREEN_SCALE
   GL_GREEN_BIAS
   GL_BLUE_SCALE
   GL_BLUE_BIAS
   GL_ALPHA_SCALE
   GL_ALPHA_BIAS
   GL_DEPTH_SCALE
   GL_DEPTH_BIAS
   GL_MAX_EVAL_ORDER
   GL_MAX_LIGHTS
   GL_MAX_CLIP_PLANES
   GL_MAX_TEXTURE_SIZE
   GL_MAX_PIXEL_MAP_TABLE
   GL_MAX_ATTRIB_STACK_DEPTH
   GL_MAX_MODELVIEW_STACK_DEPTH
   GL_MAX_NAME_STACK_DEPTH
   GL_MAX_PROJECTION_STACK_DEPTH
   GL_MAX_TEXTURE_STACK_DEPTH
   GL_MAX_VIEWPORT_DIMS
   GL_SUBPIXEL_BITS
   GL_INDEX_BITS
   GL_RED_BITS
   GL_GREEN_BITS
   GL_BLUE_BITS
   GL_ALPHA_BITS
   GL_DEPTH_BITS
   GL_STENCIL_BITS
   GL_ACCUM_RED_BITS
   GL_ACCUM_GREEN_BITS
   GL_ACCUM_BLUE_BITS
   GL_ACCUM_ALPHA_BITS
   GL_NAME_STACK_DEPTH
   GL_AUTO_NORMAL
   GL_MAP1_COLOR_4
   GL_MAP1_INDEX
   GL_MAP1_NORMAL
   GL_MAP1_TEXTURE_COORD_1
   GL_MAP1_TEXTURE_COORD_2
   GL_MAP1_TEXTURE_COORD_3
   GL_MAP1_TEXTURE_COORD_4
   GL_MAP1_VERTEX_3
   GL_MAP1_VERTEX_4
   GL_MAP2_COLOR_4
   GL_MAP2_INDEX
   GL_MAP2_NORMAL
   GL_MAP2_TEXTURE_COORD_1
   GL_MAP2_TEXTURE_COORD_2
   GL_MAP2_TEXTURE_COORD_3
   GL_MAP2_TEXTURE_COORD_4
   GL_MAP2_VERTEX_3
   GL_MAP2_VERTEX_4
   GL_MAP1_GRID_DOMAIN
   GL_MAP1_GRID_SEGMENTS
   GL_MAP2_GRID_DOMAIN
   GL_MAP2_GRID_SEGMENTS
   GL_TEXTURE_1D
   GL_TEXTURE_2D
   GL_FEEDBACK_BUFFER_POINTER
   GL_FEEDBACK_BUFFER_SIZE
   GL_FEEDBACK_BUFFER_TYPE
   GL_SELECTION_BUFFER_POINTER
   GL_SELECTION_BUFFER_SIZE

   ; GetTextureParameter
   ; GL_TEXTURE_MAG_FILTER
   ; GL_TEXTURE_MIN_FILTER
   ; GL_TEXTURE_WRAP_S
   ; GL_TEXTURE_WRAP_T
   GL_TEXTURE_WIDTH
   GL_TEXTURE_HEIGHT
   GL_TEXTURE_COMPONENTS
   GL_TEXTURE_BORDER_COLOR
   GL_TEXTURE_BORDER

   ; HintMode
   GL_DONT_CARE
   GL_FASTEST
   GL_NICEST

   ; HintTarget
   ; GL_PERSPECTIVE_CORRECTION_HINT
   ; GL_POINT_SMOOTH_HINT
   ; GL_LINE_SMOOTH_HINT
   ; GL_POLYGON_SMOOTH_HINT
   ; GL_FOG_HINT
   ; GL_PHONG_HINT

   ; IndexPointerType
   ; GL_SHORT
   ; GL_INT
   ; GL_FLOAT

   ; LightModelParameter
   ; GL_LIGHT_MODEL_AMBIENT
   ; GL_LIGHT_MODEL_LOCAL_VIEWER
   ; GL_LIGHT_MODEL_TWO_SIDE

   ; LightName
   GL_LIGHT0
   GL_LIGHT1
   GL_LIGHT2
   GL_LIGHT3
   GL_LIGHT4
   GL_LIGHT5
   GL_LIGHT6
   GL_LIGHT7

   ; LightParameter
   GL_AMBIENT
   GL_DIFFUSE
   GL_SPECULAR
   GL_POSITION
   GL_SPOT_DIRECTION
   GL_SPOT_EXPONENT
   GL_SPOT_CUTOFF
   GL_CONSTANT_ATTENUATION
   GL_LINEAR_ATTENUATION
   GL_QUADRATIC_ATTENUATION

   ; ListMode
   GL_COMPILE
   GL_COMPILE_AND_EXECUTE

   ; ListNameType
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT
   ; GL_2_BYTES
   ; GL_3_BYTES
   ; GL_4_BYTES

   ; LogicOp
   GL_CLEAR
   GL_AND
   GL_AND_REVERSE
   GL_COPY
   GL_AND_INVERTED
   GL_NOOP
   GL_XOR
   GL_OR
   GL_NOR
   GL_EQUIV
   GL_INVERT
   GL_OR_REVERSE
   GL_COPY_INVERTED
   GL_OR_INVERTED
   GL_NAND
   GL_SET

   ; MapTarget
   ; GL_MAP1_COLOR_4
   ; GL_MAP1_INDEX
   ; GL_MAP1_NORMAL
   ; GL_MAP1_TEXTURE_COORD_1
   ; GL_MAP1_TEXTURE_COORD_2
   ; GL_MAP1_TEXTURE_COORD_3
   ; GL_MAP1_TEXTURE_COORD_4
   ; GL_MAP1_VERTEX_3
   ; GL_MAP1_VERTEX_4
   ; GL_MAP2_COLOR_4
   ; GL_MAP2_INDEX
   ; GL_MAP2_NORMAL
   ; GL_MAP2_TEXTURE_COORD_1
   ; GL_MAP2_TEXTURE_COORD_2
   ; GL_MAP2_TEXTURE_COORD_3
   ; GL_MAP2_TEXTURE_COORD_4
   ; GL_MAP2_VERTEX_3
   ; GL_MAP2_VERTEX_4

   ; MaterialFace
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; MaterialParameter
   GL_EMISSION
   GL_SHININESS
   GL_AMBIENT_AND_DIFFUSE
   GL_COLOR_INDEXES
   ; GL_AMBIENT
   ; GL_DIFFUSE
   ; GL_SPECULAR

   ; MatrixMode
   GL_MODELVIEW
   GL_PROJECTION
   GL_TEXTURE

   ; MeshMode1
   ; GL_POINT
   ; GL_LINE

   ; MeshMode2
   ; GL_POINT
   ; GL_LINE
   ; GL_FILL

   ; NormalPointerType
   ; GL_BYTE
   ; GL_SHORT
   ; GL_INT
   ; GL_FLOAT

   ; PixelCopyType
   GL_COLOR
   GL_DEPTH
   GL_STENCIL

   ; PixelFormat
   GL_COLOR_INDEX
   GL_STENCIL_INDEX
   GL_DEPTH_COMPONENT
   GL_RED
   GL_GREEN
   GL_BLUE
   GL_ALPHA
   GL_RGB
   GL_RGBA
   GL_LUMINANCE
   GL_LUMINANCE_ALPHA

   ; PixelMap
   ; GL_PIXEL_MAP_I_TO_I
   ; GL_PIXEL_MAP_S_TO_S
   ; GL_PIXEL_MAP_I_TO_R
   ; GL_PIXEL_MAP_I_TO_G
   ; GL_PIXEL_MAP_I_TO_B
   ; GL_PIXEL_MAP_I_TO_A
   ; GL_PIXEL_MAP_R_TO_R
   ; GL_PIXEL_MAP_G_TO_G
   ; GL_PIXEL_MAP_B_TO_B
   ; GL_PIXEL_MAP_A_TO_A

   ; PixelStore
   ; GL_UNPACK_SWAP_BYTES
   ; GL_UNPACK_LSB_FIRST
   ; GL_UNPACK_ROW_LENGTH
   ; GL_UNPACK_SKIP_ROWS
   ; GL_UNPACK_SKIP_PIXELS
   ; GL_UNPACK_ALIGNMENT
   ; GL_PACK_SWAP_BYTES
   ; GL_PACK_LSB_FIRST
   ; GL_PACK_ROW_LENGTH
   ; GL_PACK_SKIP_ROWS
   ; GL_PACK_SKIP_PIXELS
   ; GL_PACK_ALIGNMENT

   ; PixelTransfer
   ; GL_MAP_COLOR
   ; GL_MAP_STENCIL
   ; GL_INDEX_SHIFT
   ; GL_INDEX_OFFSET
   ; GL_RED_SCALE
   ; GL_RED_BIAS
   ; GL_GREEN_SCALE
   ; GL_GREEN_BIAS
   ; GL_BLUE_SCALE
   ; GL_BLUE_BIAS
   ; GL_ALPHA_SCALE
   ; GL_ALPHA_BIAS
   ; GL_DEPTH_SCALE
   ; GL_DEPTH_BIAS

   ; PixelType
   GL_BITMAP
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT

   ; PolygonMode
   GL_POINT
   GL_LINE
   GL_FILL

   ; ReadBufferMode
   ; GL_FRONT_LEFT
   ; GL_FRONT_RIGHT
   ; GL_BACK_LEFT
   ; GL_BACK_RIGHT
   ; GL_FRONT
   ; GL_BACK
   ; GL_LEFT
   ; GL_RIGHT
   ; GL_AUX0
   ; GL_AUX1
   ; GL_AUX2
   ; GL_AUX3

   ; RenderingMode
   GL_RENDER
   GL_FEEDBACK
   GL_SELECT

   ; ShadingModel
   GL_FLAT
   GL_SMOOTH

   ; StencilFunction
   ; GL_NEVER
   ; GL_LESS
   ; GL_EQUAL
   ; GL_LEQUAL
   ; GL_GREATER
   ; GL_NOTEQUAL
   ; GL_GEQUAL
   ; GL_ALWAYS

   ; StencilOp
   ; GL_ZERO
   GL_KEEP
   GL_REPLACE
   GL_INCR
   GL_DECR
   ; GL_INVERT

   ; StringName
   GL_VENDOR
   GL_RENDERER
   GL_VERSION
   GL_EXTENSIONS

   ; TextureCoordName
   GL_S
   GL_T
   GL_R
   GL_Q

   ; TextureEnvMode
   GL_MODULATE
   GL_DECAL
   ; GL_BLEND
   ; GL_REPLACE

   ; TextureEnvParameter
   GL_TEXTURE_ENV_MODE
   GL_TEXTURE_ENV_COLOR

   ; TextureEnvTarget
   GL_TEXTURE_ENV

   ; TextureGenMode
   GL_EYE_LINEAR
   GL_OBJECT_LINEAR
   GL_SPHERE_MAP

   ; TextureGenParameter
   GL_TEXTURE_GEN_MODE
   GL_OBJECT_PLANE
   GL_EYE_PLANE

   ; TextureMagFilter
   GL_NEAREST
   GL_LINEAR

   ;TextureMinFilter
   ; GL_NEAREST
   ; GL_LINEAR
   GL_NEAREST_MIPMAP_NEAREST
   GL_LINEAR_MIPMAP_NEAREST
   GL_NEAREST_MIPMAP_LINEAR
   GL_LINEAR_MIPMAP_LINEAR

   ; TextureParameterName
   GL_TEXTURE_MAG_FILTER
   GL_TEXTURE_MIN_FILTER
   GL_TEXTURE_WRAP_S
   GL_TEXTURE_WRAP_T
   ; GL_TEXTURE_BORDER_COLOR

   ; TextureTarget
   ; GL_TEXTURE_1D
   ; GL_TEXTURE_2D

   ; TextureWrapMode
   GL_CLAMP
   GL_REPEAT

   ; --- end of OpenGL 1.0 specification ------------------------------------
   ; ------------------------------------------------------------------------

   ; openGL Utility
   GLU_LIBRARY

   GLU_VERSION_1_0
   GLU_VERSION_1_1

   gluErrorString
   gluOrtho2D
   gluPerspective
   gluLookAt

   gluNewQuadric gluDeleteQuadric gluQuadricDrawStyle
      GLU_FILL GLU_LINE GLU_SILHOUETTE GLU_POINT
   gluQuadricOrientation
      GLU_OUTSIDE GLU_INSIDE

   gluSphere
   gluCylinder

;   gluNewTess gluBeginPolygon gluTessVertex

   gluNewNurbsRenderer gluNurbsSurface gluBeginSurface gluEndSurface
   gluNurbsProperty
      GLU_OUTLINE_POLYGON GLU_OUTLINE_PATCH

   GLU_U_STEP GLU_V_STEP GLU_DISPLAY_MODE

   ; platform staff
   (exports (OpenGL platform)))

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL platform))

(begin
   (define GL_VERSION_1_0 1)

   (setq GL GL_LIBRARY)
   ; -------------------------------------------------------------------------
   ; constants

   (define GL_TRUE  1)
   (define GL_FALSE 0)

   ; AccumOp
   (define GL_ACCUM               #x0100)
   (define GL_LOAD                #x0101)
   (define GL_RETURN              #x0102)
   (define GL_MULT                #x0103)
   (define GL_ADD                 #x0104)

   ; AlphaFunction
   (define GL_NEVER               #x0200)
   (define GL_LESS                #x0201)
   (define GL_EQUAL               #x0202)
   (define GL_LEQUAL              #x0203)
   (define GL_GREATER             #x0204)
   (define GL_NOTEQUAL            #x0205)
   (define GL_GEQUAL              #x0206)
   (define GL_ALWAYS              #x0207)

   ; AttribMask
   (define GL_CURRENT_BIT         #x00001)
   (define GL_POINT_BIT           #x00002)
   (define GL_LINE_BIT            #x00004)
   (define GL_POLYGON_BIT         #x00008)
   (define GL_POLYGON_STIPPLE_BIT #x00010)
   (define GL_PIXEL_MODE_BIT      #x00020)
   (define GL_LIGHTING_BIT        #x00040)
   (define GL_FOG_BIT             #x00080)
   (define GL_DEPTH_BUFFER_BIT    #x00100)
   (define GL_ACCUM_BUFFER_BIT    #x00200)
   (define GL_STENCIL_BUFFER_BIT  #x00400)
   (define GL_VIEWPORT_BIT        #x00800)
   (define GL_TRANSFORM_BIT       #x01000)
   (define GL_ENABLE_BIT          #x02000)
   (define GL_COLOR_BUFFER_BIT    #x04000)
   (define GL_HINT_BIT            #x08000)
   (define GL_EVAL_BIT            #x10000)
   (define GL_LIST_BIT            #x20000)
   (define GL_TEXTURE_BIT         #x40000)
   (define GL_SCISSOR_BIT         #x80000)
   (define GL_ALL_ATTRIB_BITS     #xFFFFF)

   ; BeginMode
   (define GL_POINTS              #x0000)
   (define GL_LINES               #x0001)
   (define GL_LINE_LOOP           #x0002)
   (define GL_LINE_STRIP          #x0003)
   (define GL_TRIANGLES           #x0004)
   (define GL_TRIANGLE_STRIP      #x0005)
   (define GL_TRIANGLE_FAN        #x0006)
   (define GL_QUADS               #x0007)
   (define GL_QUAD_STRIP          #x0008)
   (define GL_POLYGON             #x0009)

   ; BlendingFactorDest
   (define GL_ZERO                0)
   (define GL_ONE                 1)
   (define GL_SRC_COLOR           #x0300)
   (define GL_ONE_MINUS_SRC_COLOR #x0301)
   (define GL_SRC_ALPHA           #x0302)
   (define GL_ONE_MINUS_SRC_ALPHA #x0303)
   (define GL_DST_ALPHA           #x0304)
   (define GL_ONE_MINUS_DST_ALPHA #x0305)

   ; BlendingFactorSrc
   ; GL_ZERO
   ; GL_ONE
   (define GL_DST_COLOR           #x0306)
   (define GL_ONE_MINUS_DST_COLOR #x0307)
   (define GL_SRC_ALPHA_SATURATE  #x0308)
   ; GL_SRC_ALPHA
   ; GL_ONE_MINUS_SRC_ALPHA
   ; GL_DST_ALPHA
   ; GL_ONE_MINUS_DST_ALPHA

   ; ClearBufferMask
   ; GL_COLOR_BUFFER_BIT
   ; GL_ACCUM_BUFFER_BIT
   ; GL_STENCIL_BUFFER_BIT
   ; GL_DEPTH_BUFFER_BIT

   ; ClipPlaneName
   (define GL_CLIP_PLANE0 #x3000)
   (define GL_CLIP_PLANE1 #x3001)
   (define GL_CLIP_PLANE2 #x3002)
   (define GL_CLIP_PLANE3 #x3003)
   (define GL_CLIP_PLANE4 #x3004)
   (define GL_CLIP_PLANE5 #x3005)

   ; ColorMaterialFace
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; ColorMaterialParameter
   ; GL_AMBIENT
   ; GL_DIFFUSE
   ; GL_SPECULAR
   ; GL_EMISSION
   ; GL_AMBIENT_AND_DIFFUSE

   ; ColorPointerType
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT

   ; CullFaceMode
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; DataType
   (define GL_BYTE                #x1400)
   (define GL_UNSIGNED_BYTE       #x1401)
   (define GL_SHORT               #x1402)
   (define GL_UNSIGNED_SHORT      #x1403)
   (define GL_INT                 #x1404)
   (define GL_UNSIGNED_INT        #x1405)
   (define GL_FLOAT               #x1406)
   (define GL_2_BYTES             #x1407)
   (define GL_3_BYTES             #x1408)
   (define GL_4_BYTES             #x1409)

   ; DepthFunction
   ; GL_NEVER
   ; GL_LESS
   ; GL_EQUAL
   ; GL_LEQUAL
   ; GL_GREATER
   ; GL_NOTEQUAL
   ; GL_GEQUAL
   ; GL_ALWAYS

   ; DrawBufferMode
   (define GL_NONE 0)
   (define GL_FRONT_LEFT     #x0400)
   (define GL_FRONT_RIGHT    #x0401)
   (define GL_BACK_LEFT      #x0402)
   (define GL_BACK_RIGHT     #x0403)
   (define GL_FRONT          #x0404)
   (define GL_BACK           #x0405)
   (define GL_LEFT           #x0406)
   (define GL_RIGHT          #x0407)
   (define GL_FRONT_AND_BACK #x0408)
   (define GL_AUX0           #x0409)
   (define GL_AUX1           #x040A)
   (define GL_AUX2           #x040B)
   (define GL_AUX3           #x040C)

   ; Enable
   ; GL_FOG
   ; GL_LIGHTING
   ; GL_TEXTURE_1D
   ; GL_TEXTURE_2D
   ; GL_LINE_STIPPLE
   ; GL_POLYGON_STIPPLE
   ; GL_CULL_FACE
   ; GL_ALPHA_TEST
   ; GL_BLEND
   ; GL_LOGIC_OP
   ; GL_COLOR_LOGIC_OP
   ; GL_DITHER
   ; GL_STENCIL_TEST
   ; GL_DEPTH_TEST
   ; GL_CLIP_PLANE0
   ; GL_CLIP_PLANE1
   ; GL_CLIP_PLANE2
   ; GL_CLIP_PLANE3
   ; GL_CLIP_PLANE4
   ; GL_CLIP_PLANE5
   ; GL_LIGHT0
   ; GL_LIGHT1
   ; GL_LIGHT2
   ; GL_LIGHT3
   ; GL_LIGHT4
   ; GL_LIGHT5
   ; GL_LIGHT6
   ; GL_LIGHT7
   ; GL_TEXTURE_GEN_S
   ; GL_TEXTURE_GEN_T
   ; GL_TEXTURE_GEN_R
   ; GL_TEXTURE_GEN_Q
   ; GL_MAP1_VERTEX_3
   ; GL_MAP1_VERTEX_4
   ; GL_MAP1_COLOR_4
   ; GL_MAP1_INDEX
   ; GL_MAP1_NORMAL
   ; GL_MAP1_TEXTURE_COORD_1
   ; GL_MAP1_TEXTURE_COORD_2
   ; GL_MAP1_TEXTURE_COORD_3
   ; GL_MAP1_TEXTURE_COORD_4
   ; GL_MAP2_VERTEX_3
   ; GL_MAP2_VERTEX_4
   ; GL_MAP2_COLOR_4
   ; GL_MAP2_INDEX
   ; GL_MAP2_NORMAL
   ; GL_MAP2_TEXTURE_COORD_1
   ; GL_MAP2_TEXTURE_COORD_2
   ; GL_MAP2_TEXTURE_COORD_3
   ; GL_MAP2_TEXTURE_COORD_4
   ; GL_POINT_SMOOTH
   ; GL_LINE_SMOOTH
   ; GL_POLYGON_SMOOTH
   ; GL_SCISSOR_TEST
   ; GL_COLOR_MATERIAL
   ; GL_NORMALIZE
   ; GL_AUTO_NORMAL

   ; ErrorCode
   (define GL_NO_ERROR 0)
   (define GL_INVALID_ENUM      #x0500)
   (define GL_INVALID_VALUE     #x0501)
   (define GL_INVALID_OPERATION #x0502)
   (define GL_STACK_OVERFLOW    #x0503)
   (define GL_STACK_UNDERFLOW   #x0504)
   (define GL_OUT_OF_MEMORY     #x0505)

   ; FeedBackMode
   (define GL_2D #x0600)
   (define GL_3D #x0601)
   (define GL_3D_COLOR #x0602)
   (define GL_3D_COLOR_TEXTURE #x0603)
   (define GL_4D_COLOR_TEXTURE #x0604)

   ; FeedBackToken
   (define GL_PASS_THROUGH_TOKEN #x0700)
   (define GL_POINT_TOKEN #x0701)
   (define GL_LINE_TOKEN #x0702)
   (define GL_POLYGON_TOKEN #x0703)
   (define GL_BITMAP_TOKEN #x0704)
   (define GL_DRAW_PIXEL_TOKEN #x0705)
   (define GL_COPY_PIXEL_TOKEN #x0706)
   (define GL_LINE_RESET_TOKEN #x0707)

   ; FogMode
   ; GL_LINEAR
   (define GL_EXP #x0800)
   (define GL_EXP2 #x0801)

   ; FogParameter
   ; GL_FOG_COLOR
   ; GL_FOG_DENSITY
   ; GL_FOG_END
   ; GL_FOG_INDEX
   ; GL_FOG_MODE
   ; GL_FOG_START

   ; FrontFaceDirection
   (define GL_CW #x0900)
   (define GL_CCW #x0901)

   ; GetMapTarget
   (define GL_COEFF #x0A00)
   (define GL_ORDER #x0A01)
   (define GL_DOMAIN #x0A02)

   ; GetPixelMap
   ; GL_PIXEL_MAP_I_TO_I
   ; GL_PIXEL_MAP_S_TO_S
   ; GL_PIXEL_MAP_I_TO_R
   ; GL_PIXEL_MAP_I_TO_G
   ; GL_PIXEL_MAP_I_TO_B
   ; GL_PIXEL_MAP_I_TO_A
   ; GL_PIXEL_MAP_R_TO_R
   ; GL_PIXEL_MAP_G_TO_G
   ; GL_PIXEL_MAP_B_TO_B
   ; GL_PIXEL_MAP_A_TO_A

   ; GetTarget
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
   (define GL_TEXTURE_STACK_DEPTH #x0BA5)
   (define GL_MODELVIEW_MATRIX #x0BA6)
   (define GL_PROJECTION_MATRIX #x0BA7)
   (define GL_TEXTURE_MATRIX #x0BA8)
   (define GL_ATTRIB_STACK_DEPTH #x0BB0)
   (define GL_ALPHA_TEST #x0BC0)
   (define GL_ALPHA_TEST_FUNC #x0BC1)
   (define GL_ALPHA_TEST_REF #x0BC2)
   (define GL_DITHER #x0BD0)
   (define GL_BLEND_DST #x0BE0)
   (define GL_BLEND_SRC #x0BE1)
   (define GL_BLEND #x0BE2)
   (define GL_LOGIC_OP_MODE #x0BF0)
   (define GL_LOGIC_OP #x0BF1)
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

   ; GetTextureParameter
   ; GL_TEXTURE_MAG_FILTER
   ; GL_TEXTURE_MIN_FILTER
   ; GL_TEXTURE_WRAP_S
   ; GL_TEXTURE_WRAP_T
   (define GL_TEXTURE_WIDTH #x1000)
   (define GL_TEXTURE_HEIGHT #x1001)
   (define GL_TEXTURE_COMPONENTS #x1003)
   (define GL_TEXTURE_BORDER_COLOR #x1004)
   (define GL_TEXTURE_BORDER #x1005)

   ; HintMode
   (define GL_DONT_CARE #x1100)
   (define GL_FASTEST #x1101)
   (define GL_NICEST #x1102)

   ; HintTarget
   ; GL_PERSPECTIVE_CORRECTION_HINT
   ; GL_POINT_SMOOTH_HINT
   ; GL_LINE_SMOOTH_HINT
   ; GL_POLYGON_SMOOTH_HINT
   ; GL_FOG_HINT
   ; GL_PHONG_HINT

   ; IndexPointerType
   ; GL_SHORT
   ; GL_INT
   ; GL_FLOAT

   ; LightModelParameter
   ; GL_LIGHT_MODEL_AMBIENT
   ; GL_LIGHT_MODEL_LOCAL_VIEWER
   ; GL_LIGHT_MODEL_TWO_SIDE

   ; LightName
   (define GL_LIGHT0 #x4000)
   (define GL_LIGHT1 #x4001)
   (define GL_LIGHT2 #x4002)
   (define GL_LIGHT3 #x4003)
   (define GL_LIGHT4 #x4004)
   (define GL_LIGHT5 #x4005)
   (define GL_LIGHT6 #x4006)
   (define GL_LIGHT7 #x4007)

   ; LightParameter
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

   ; ListMode
   (define GL_COMPILE #x1300)
   (define GL_COMPILE_AND_EXECUTE #x1301)

   ; ListNameType
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT
   ; GL_2_BYTES
   ; GL_3_BYTES
   ; GL_4_BYTES

   ; LogicOp
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

   ; MapTarget
   ; GL_MAP1_COLOR_4
   ; GL_MAP1_INDEX
   ; GL_MAP1_NORMAL
   ; GL_MAP1_TEXTURE_COORD_1
   ; GL_MAP1_TEXTURE_COORD_2
   ; GL_MAP1_TEXTURE_COORD_3
   ; GL_MAP1_TEXTURE_COORD_4
   ; GL_MAP1_VERTEX_3
   ; GL_MAP1_VERTEX_4
   ; GL_MAP2_COLOR_4
   ; GL_MAP2_INDEX
   ; GL_MAP2_NORMAL
   ; GL_MAP2_TEXTURE_COORD_1
   ; GL_MAP2_TEXTURE_COORD_2
   ; GL_MAP2_TEXTURE_COORD_3
   ; GL_MAP2_TEXTURE_COORD_4
   ; GL_MAP2_VERTEX_3
   ; GL_MAP2_VERTEX_4

   ; MaterialFace
   ; GL_FRONT
   ; GL_BACK
   ; GL_FRONT_AND_BACK

   ; MaterialParameter
   (define GL_EMISSION #x1600)
   (define GL_SHININESS #x1601)
   (define GL_AMBIENT_AND_DIFFUSE #x1602)
   (define GL_COLOR_INDEXES #x1603)
   ; GL_AMBIENT
   ; GL_DIFFUSE
   ; GL_SPECULAR

   ; MatrixMode
   (define GL_MODELVIEW #x1700)
   (define GL_PROJECTION #x1701)
   (define GL_TEXTURE #x1702)

   ; MeshMode1
   ; GL_POINT
   ; GL_LINE

   ; MeshMode2
   ; GL_POINT
   ; GL_LINE
   ; GL_FILL

   ; NormalPointerType
   ; GL_BYTE
   ; GL_SHORT
   ; GL_INT
   ; GL_FLOAT

   ; PixelCopyType
   (define GL_COLOR #x1800)
   (define GL_DEPTH #x1801)
   (define GL_STENCIL #x1802)

   ; PixelFormat
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

   ; PixelMap
   ; GL_PIXEL_MAP_I_TO_I
   ; GL_PIXEL_MAP_S_TO_S
   ; GL_PIXEL_MAP_I_TO_R
   ; GL_PIXEL_MAP_I_TO_G
   ; GL_PIXEL_MAP_I_TO_B
   ; GL_PIXEL_MAP_I_TO_A
   ; GL_PIXEL_MAP_R_TO_R
   ; GL_PIXEL_MAP_G_TO_G
   ; GL_PIXEL_MAP_B_TO_B
   ; GL_PIXEL_MAP_A_TO_A

   ; PixelStore
   ; GL_UNPACK_SWAP_BYTES
   ; GL_UNPACK_LSB_FIRST
   ; GL_UNPACK_ROW_LENGTH
   ; GL_UNPACK_SKIP_ROWS
   ; GL_UNPACK_SKIP_PIXELS
   ; GL_UNPACK_ALIGNMENT
   ; GL_PACK_SWAP_BYTES
   ; GL_PACK_LSB_FIRST
   ; GL_PACK_ROW_LENGTH
   ; GL_PACK_SKIP_ROWS
   ; GL_PACK_SKIP_PIXELS
   ; GL_PACK_ALIGNMENT

   ; PixelTransfer
   ; GL_MAP_COLOR
   ; GL_MAP_STENCIL
   ; GL_INDEX_SHIFT
   ; GL_INDEX_OFFSET
   ; GL_RED_SCALE
   ; GL_RED_BIAS
   ; GL_GREEN_SCALE
   ; GL_GREEN_BIAS
   ; GL_BLUE_SCALE
   ; GL_BLUE_BIAS
   ; GL_ALPHA_SCALE
   ; GL_ALPHA_BIAS
   ; GL_DEPTH_SCALE
   ; GL_DEPTH_BIAS

   ; PixelType
   (define GL_BITMAP #x1A00)
   ; GL_BYTE
   ; GL_UNSIGNED_BYTE
   ; GL_SHORT
   ; GL_UNSIGNED_SHORT
   ; GL_INT
   ; GL_UNSIGNED_INT
   ; GL_FLOAT

   ; PolygonMode
   (define GL_POINT #x1B00)
   (define GL_LINE #x1B01)
   (define GL_FILL #x1B02)

   ; ReadBufferMode
   ; GL_FRONT_LEFT
   ; GL_FRONT_RIGHT
   ; GL_BACK_LEFT
   ; GL_BACK_RIGHT
   ; GL_FRONT
   ; GL_BACK
   ; GL_LEFT
   ; GL_RIGHT
   ; GL_AUX0
   ; GL_AUX1
   ; GL_AUX2
   ; GL_AUX3

   ; RenderingMode
   (define GL_RENDER #x1C00)
   (define GL_FEEDBACK #x1C01)
   (define GL_SELECT #x1C02)

   ; ShadingModel
   (define GL_FLAT #x1D00)
   (define GL_SMOOTH #x1D01)

   ; StencilFunction
   ; GL_NEVER
   ; GL_LESS
   ; GL_EQUAL
   ; GL_LEQUAL
   ; GL_GREATER
   ; GL_NOTEQUAL
   ; GL_GEQUAL
   ; GL_ALWAYS

   ; StencilOp
   ; GL_ZERO
   (define GL_KEEP #x1E00)
   (define GL_REPLACE #x1E01)
   (define GL_INCR #x1E02)
   (define GL_DECR #x1E03)
   ; GL_INVERT

   ; StringName
   (define GL_VENDOR #x1F00)
   (define GL_RENDERER #x1F01)
   (define GL_VERSION #x1F02)
   (define GL_EXTENSIONS #x1F03)

   ; TextureCoordName
   (define GL_S #x2000)
   (define GL_T #x2001)
   (define GL_R #x2002)
   (define GL_Q #x2003)

   ; TextureEnvMode
   (define GL_MODULATE #x2100)
   (define GL_DECAL #x2101)
   ; GL_BLEND
   ; GL_REPLACE

   ; TextureEnvParameter
   (define GL_TEXTURE_ENV_MODE #x2200)
   (define GL_TEXTURE_ENV_COLOR #x2201)

   ; TextureEnvTarget
   (define GL_TEXTURE_ENV #x2300)

   ; TextureGenMode
   (define GL_EYE_LINEAR #x2400)
   (define GL_OBJECT_LINEAR #x2401)
   (define GL_SPHERE_MAP #x2402)

   ; TextureGenParameter
   (define GL_TEXTURE_GEN_MODE #x2500)
   (define GL_OBJECT_PLANE #x2501)
   (define GL_EYE_PLANE #x2502)

   ; TextureMagFilter
   (define GL_NEAREST #x2600)
   (define GL_LINEAR #x2601)

   ;TextureMinFilter
   ; GL_NEAREST
   ; GL_LINEAR
   (define GL_NEAREST_MIPMAP_NEAREST #x2700)
   (define GL_LINEAR_MIPMAP_NEAREST #x2701)
   (define GL_NEAREST_MIPMAP_LINEAR #x2702)
   (define GL_LINEAR_MIPMAP_LINEAR #x2703)

   ; TextureParameterName
   (define GL_TEXTURE_MAG_FILTER #x2800)
   (define GL_TEXTURE_MIN_FILTER #x2801)
   (define GL_TEXTURE_WRAP_S #x2802)
   (define GL_TEXTURE_WRAP_T #x2803)
   ; GL_TEXTURE_BORDER_COLOR

   ; TextureTarget
   ; GL_TEXTURE_1D
   ; GL_TEXTURE_2D

   ; TextureWrapMode
   (define GL_CLAMP #x2900)
   (define GL_REPEAT #x2901)


   ; functions
   (define glAccum       (GL GLvoid "glAccum"      GLenum GLfloat))
   (define glAlphaFunc   (GL GLvoid "glAlphaFunc"  GLenum GLclampf))
   (define glBegin       (GL GLvoid "glBegin"      GLenum))
   (define glBitmap      (GL GLvoid "glBitmap"     GLsizei GLsizei GLfloat GLfloat GLfloat GLfloat GLubyte*))
   (define glBlendFunc   (GL GLvoid "glBlendFunc"  GLenum GLenum))
   (define glCallList    (GL GLvoid "glCallList"   GLuint))
   (define glCallLists   (GL GLvoid "glCallLists"  GLsizei GLenum GLvoid*))
   (define glClear       (GL GLvoid "glClear"      GLbitfield))
   (define glClearAccum  (GL GLvoid "glClearAccum" GLfloat GLfloat GLfloat GLfloat))
   (define glClearColor  (GL GLvoid "glClearColor" GLfloat GLfloat GLfloat GLfloat))
   (define glClearDepth  (GL GLvoid "glClearDepth" GLclampd))
   (define glClearIndex  (GL GLvoid "glClearIndex" GLfloat))
   (define glClearStencil(GL GLvoid "glClearStencil" GLint))
   (define glClipPlane   (GL GLvoid "glClipPlane"  GLenum GLdouble*))

   (define glColor3b     (GL GLvoid "glColor3b"    GLbyte GLbyte GLbyte))
   (define glColor3bv    (GL GLvoid "glColor3bv"   GLbyte*))
   (define glColor3d     (GL GLvoid "glColor3d"    GLdouble GLdouble GLdouble))
   (define glColor3dv    (GL GLvoid "glColor3dv"   GLdouble*))
   (define glColor3f     (GL GLvoid "glColor3f"    GLfloat GLfloat GLfloat))
   (define glColor3fv    (GL GLvoid "glColor3fv"   GLfloat*))
   (define glColor3i     (GL GLvoid "glColor3i"    GLint GLint GLint))
   (define glColor3iv    (GL GLvoid "glColor3iv"   GLint*))
   (define glColor3s     (GL GLvoid "glColor3s"    GLshort GLshort GLshort))
   (define glColor3sv    (GL GLvoid "glColor3sv"   GLshort*))
   (define glColor3ub    (GL GLvoid "glColor3ub"   GLubyte GLubyte GLubyte))
   (define glColor3ubv   (GL GLvoid "glColor3ubv"  GLubyte*))
   (define glColor3ui    (GL GLvoid "glColor3ui"   GLuint GLuint GLuint))
   (define glColor3uiv   (GL GLvoid "glColor3uiv"  GLuint*))
   (define glColor3us    (GL GLvoid "glColor3us"   GLushort GLushort GLushort))
   (define glColor3usv   (GL GLvoid "glColor3usv"  GLushort*))
   (define glColor4b     (GL GLvoid "glColor4b"    GLbyte GLbyte GLbyte GLbyte))
   (define glColor4bv    (GL GLvoid "glColor4bv"   GLbyte*))
   (define glColor4d     (GL GLvoid "glColor4d"    GLdouble GLdouble GLdouble GLdouble))
   (define glColor4dv    (GL GLvoid "glColor4dv"   GLdouble*))
   (define glColor4f     (GL GLvoid "glColor4f"    GLfloat GLfloat GLfloat GLfloat))
   (define glColor4fv    (GL GLvoid "glColor4fv"   GLfloat*))
   (define glColor4i     (GL GLvoid "glColor4i"    GLint GLint GLint GLint))
   (define glColor4iv    (GL GLvoid "glColor4iv"   GLint*))
   (define glColor4s     (GL GLvoid "glColor4s"    GLshort GLshort GLshort GLshort))
   (define glColor4sv    (GL GLvoid "glColor4sv"   GLshort*))
   (define glColor4ub    (GL GLvoid "glColor4ub"   GLubyte GLubyte GLubyte GLubyte))
   (define glColor4ubv   (GL GLvoid "glColor4ubv"  GLubyte*))
   (define glColor4ui    (GL GLvoid "glColor4ui"   GLuint GLuint GLuint GLuint))
   (define glColor4uiv   (GL GLvoid "glColor4uiv"  GLuint*))
   (define glColor4us    (GL GLvoid "glColor4us"   GLushort GLushort GLushort GLushort))
   (define glColor4usv   (GL GLvoid "glColor4usv"  GLushort*))

   (define glColorMask   (GL GLvoid "glColorMask"  GLboolean GLboolean GLboolean GLboolean))
   (define glColorMaterial (GL GLvoid "glColorMaterial" GLenum GLenum))
   (define glCopyPixels  (GL GLvoid "glCopyPixels" GLint GLint GLsizei GLsizei GLenum))
   (define glCullFace    (GL GLvoid "glCullFace"   GLenum))
   (define glDeleteLists (GL GLvoid "glDeleteLists" GLuint GLsizei))
   (define glDepthFunc   (GL GLvoid "glDepthFunc"  GLenum))
   (define glDepthMask   (GL GLvoid "glDepthMask"  GLboolean))
   (define glDepthRange  (GL GLvoid "glDepthRange" GLclampd GLclampd))
   (define glDisable     (GL GLvoid "glDisable"    GLenum))
   (define glDrawBuffer  (GL GLvoid "glDrawBuffer" GLenum))
   (define glDrawPixels  (GL GLvoid "glDrawPixels" GLsizei GLsizei GLenum GLenum GLvoid*))
   (define glEdgeFlag    (GL GLvoid "glEdgeFlag"   GLboolean))
   (define glEdgeFlagv   (GL GLvoid "glEdgeFlagv"  GLboolean*))
   (define glEnable      (GL GLvoid "glEnable"     GLenum))
   (define glEnd         (GL GLvoid "glEnd"))
   (define glEndList     (GL GLvoid "glEndList"))
   (define glEvalCoord1d (GL GLvoid "glEvalCoord1d" GLdouble))
   (define glEvalCoord1dv(GL GLvoid "glEvalCoord1dv" GLdouble*))
   (define glEvalCoord1f (GL GLvoid "glEvalCoord1f" GLfloat))
   (define glEvalCoord1fv(GL GLvoid "glEvalCoord1fv" GLfloat*))
   (define glEvalCoord2d (GL GLvoid "glEvalCoord2d" GLdouble GLdouble))
   (define glEvalCoord2dv(GL GLvoid "glEvalCoord2dv" GLdouble*))
   (define glEvalCoord2f (GL GLvoid "glEvalCoord2f" GLfloat GLfloat))
   (define glEvalCoord2fv(GL GLvoid "glEvalCoord2fv" GLfloat*))
   (define glEvalMesh1   (GL GLvoid "glEvalMesh1" GLenum GLint GLint))
   (define glEvalMesh2   (GL GLvoid "glEvalMesh2" GLenum GLint GLint GLint GLint))
   (define glEvalPoint1  (GL GLvoid "glEvalPoint1" GLint))
   (define glEvalPoint2  (GL GLvoid "glEvalPoint2" GLint GLint))
   (define glFeedbackBuffer (GL GLvoid "glFeedbackBuffer" GLsizei GLenum GLfloat*))
   (define glFinish      (GL GLvoid "glFinish"))
   (define glFlush       (GL GLvoid "glFlush"))
   (define glFogf        (GL GLvoid "glFogf" GLenum GLfloat))
   (define glFogfv       (GL GLvoid "glFogfv" GLenum GLfloat*))
   (define glFogi        (GL GLvoid "glFogi" GLenum GLint))
   (define glFogiv       (GL GLvoid "glFogiv" GLenum GLint*))
   (define glFrontFace   (GL GLvoid "glFrontFace" GLenum))
   (define glFrustum     (GL GLvoid "glFrustum" GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble))
   (define glGenLists    (GL GLuint "glGenLists" GLsizei))
   (define glGetBooleanv (GL GLvoid "glGetBooleanv" GLenum (fft& GLboolean)))
   (define glGetDoublev  (GL GLvoid "glGetDoublev"  GLenum (fft& GLdouble)))
   (define glGetError    (GL GLenum "glGetError"))
   (define glGetFloatv   (GL GLvoid "glGetFloatv"   GLenum (fft& GLfloat)))
   (define glGetIntegerv (GL GLvoid "glGetIntegerv" GLenum GLint&))
   (define glGetClipPlane(GL GLvoid "glGetClipPlane" GLenum GLdouble*))
   (define glGetLightfv  (GL GLvoid "glGetLightfv" GLenum GLenum GLfloat*))
   (define glGetLightiv  (GL GLvoid "glGetLightiv" GLenum GLenum GLint*))
   (define glGetMapdv    (GL GLvoid "glGetMapdv" GLenum GLenum GLdouble*))
   (define glGetMapfv    (GL GLvoid "glGetMapfv" GLenum GLenum GLfloat*))
   (define glGetMapiv    (GL GLvoid "glGetMapiv" GLenum GLenum GLint*))
   (define glGetMaterialfv (GL GLvoid "glGetMaterialfv" GLenum GLenum GLfloat*))
   (define glGetMaterialiv (GL GLvoid "glGetMaterialiv" GLenum GLenum GLint*))
   (define glGetPixelMapfv (GL GLvoid "glGetPixelMapfv" GLenum GLfloat*))
   (define glGetPixelMapuiv(GL GLvoid "glGetPixelMapuiv" GLenum GLuint*))
   (define glGetPixelMapusv(GL GLvoid "glGetPixelMapusv" GLenum GLushort*))
   (define glGetPolygonStipple (GL GLvoid "glGetPolygonStipple" GLubyte*))
   (define glGetString   (GL GLubyte* "glGetString" GLenum))
   (define glGetTexEnvfv (GL GLvoid "glGetTexEnvfv" GLenum GLenum GLfloat*))
   (define glGetTexEnviv (GL GLvoid "glGetTexEnviv" GLenum GLenum GLint*))
   (define glGetTexGendv (GL GLvoid "glGetTexGendv" GLenum GLenum GLdouble*))
   (define glGetTexGenfv (GL GLvoid "glGetTexGenfv" GLenum GLenum GLfloat*))
   (define glGetTexGeniv (GL GLvoid "glGetTexGeniv" GLenum GLenum GLint*))
   (define glGetTexImage (GL GLvoid "glGetTexImage" GLenum GLint GLenum GLenum GLvoid*))
   (define glGetTexLevelParameterfv (GL GLvoid "glGetTexLevelParameterfv" GLenum GLint GLenum GLfloat*))
   (define glGetTexLevelParameteriv (GL GLvoid "glGetTexLevelParameteriv" GLenum GLint GLenum GLint*))
   (define glGetTexParameteriv (GL GLvoid "glGetTexParameteriv" GLenum GLenum GLint*))
   (define glGetTexParameterfv (GL GLvoid "glGetTexParameteriv" GLenum GLenum GLfloat*))
   (define glHint        (GL GLvoid "glHint" GLenum GLenum))
   (define glIndexMask   (GL GLvoid "glIndexMask" GLuint))
   (define glIndexd      (GL GLvoid "glIndexd" GLdouble))
   (define glIndexdv     (GL GLvoid "glIndexdv" GLdouble*))
   (define glIndexf      (GL GLvoid "glIndexf" GLfloat))
   (define glIndexfv     (GL GLvoid "glIndexfv" GLfloat*))
   (define glIndexi      (GL GLvoid "glIndexi" GLint))
   (define glIndexiv     (GL GLvoid "glIndexiv" GLint*))
   (define glIndexs      (GL GLvoid "glIndexs" GLshort))
   (define glIndexsv     (GL GLvoid "glIndexsv" GLshort*))
   (define glInitNames   (GL GLvoid "glInitNames"))
   (define glIsEnabled   (GL GLboolean "glIsEnabled" GLenum))
   (define glIsList      (GL GLboolean "glIsList" GLuint))
   (define glLightModelf (GL GLvoid "glLightModelf" GLenum GLfloat))
   (define glLightModelfv(GL GLvoid "glLightModelfv" GLenum GLfloat*))
   (define glLightModeli (GL GLvoid "glLightModeli" GLenum GLint))
   (define glLightModeliv(GL GLvoid "glLightModeliv" GLenum GLint*))
   (define glLightf      (GL GLvoid "glLightf" GLenum GLenum GLfloat))
   (define glLightfv     (GL GLvoid "glLightfv" GLenum GLenum GLfloat*))
   (define glLighti      (GL GLvoid "glLighti" GLenum GLenum GLint))
   (define glLightiv     (GL GLvoid "glLightiv" GLenum GLenum GLint*))
   (define glLineStipple (GL GLvoid "glLineStipple" GLint GLushort))
   (define glLineWidth   (GL GLvoid "glLineWidth" GLfloat))
   (define glListBase    (GL GLvoid "glListBase" GLuint))
   (define glLoadIdentity(GL GLvoid "glLoadIdentity"))
   (define glLoadMatrixd (GL GLvoid "glLoadMatrixd" GLdouble*))
   (define glLoadMatrixf (GL GLvoid "glLoadMatrixf" GLfloat*))
   (define glLoadName    (GL GLvoid "glLoadName" GLuint))
   (define glLogicOp     (GL GLvoid "glLogicOp" GLenum))
   (define glMap1d       (GL GLvoid "glMap1d" GLenum GLdouble GLdouble GLint GLint GLdouble*))
   (define glMap1f       (GL GLvoid "glMap1f" GLenum GLfloat GLfloat GLint GLint GLfloat*))
   (define glMap2d       (GL GLvoid "glMap2d" GLenum GLdouble GLdouble GLint GLint GLdouble GLdouble GLint GLint GLdouble*))
   (define glMap2f       (GL GLvoid "glMap2f" GLenum GLfloat GLfloat GLint GLint GLfloat GLfloat GLint GLint GLfloat*))
   (define glMapGrid1d   (GL GLvoid "glMapGrid1d" GLint GLdouble GLdouble))
   (define glMapGrid1f   (GL GLvoid "glMapGrid1f" GLint GLfloat GLfloat))
   (define glMapGrid2d   (GL GLvoid "glMapGrid2d" GLint GLdouble GLdouble GLint GLdouble GLdouble))
   (define glMapGrid2f   (GL GLvoid "glMapGrid2f" GLint GLfloat GLfloat GLint GLfloat GLfloat))
   (define glMaterialf   (GL GLvoid "glMaterialf"  GLenum GLenum GLfloat))
   (define glMaterialfv  (GL GLvoid "glMaterialfv" GLenum GLenum GLfloat*))
   (define glMateriali   (GL GLvoid "glMateriali"  GLenum GLenum GLint))
   (define glMaterialiv  (GL GLvoid "glMaterialiv" GLenum GLenum GLint*))
   (define glMatrixMode  (GL GLvoid "glMatrixMode" GLenum))
   (define glMultMatrixd (GL GLvoid "glMultMatrixd" GLdouble*))
   (define glMultMatrixf (GL GLvoid "glMultMatrixf" GLfloat*))
   (define glNewList     (GL GLvoid "glNewList" GLuint GLenum))
   (define glNormal3b    (GL GLvoid "glNormal3b" GLbyte GLbyte GLbyte))
   (define glNormal3bv   (GL GLvoid "glNormal3bv" GLbyte*))
   (define glNormal3d    (GL GLvoid "glNormal3d" GLdouble GLdouble GLdouble))
   (define glNormal3dv   (GL GLvoid "glNormal3dv" GLdouble*))
   (define glNormal3f    (GL GLvoid "glNormal3f"  GLfloat GLfloat GLfloat))
   (define glNormal3fv   (GL GLvoid "glNormal3fv" GLfloat*))
   (define glNormal3i    (GL GLvoid "glNormal3i" GLint GLint GLint))
   (define glNormal3iv   (GL GLvoid "glNormal3iv" GLint*))
   (define glNormal3s    (GL GLvoid "glNormal3s" GLshort GLshort GLshort))
   (define glNormal3sv   (GL GLvoid "glNormal3sv" GLshort*))
   (define glOrtho       (GL GLvoid "glOrtho" GLdouble GLdouble  GLdouble GLdouble  GLdouble GLdouble))
   (define glPassThrough (GL GLvoid "glPassThrough" GLfloat))
   (define glPixelMapfv  (GL GLvoid "glPixelMapfv" GLenum GLsizei GLfloat*))
   (define glPixelMapuiv (GL GLvoid "glPixelMapuiv" GLenum GLsizei GLuint*))
   (define glPixelMapusv (GL GLvoid "glPixelMapusv" GLenum GLsizei GLushort*))
   (define glPixelTransferf (GL GLvoid "glPixelTransferf" GLenum GLfloat))
   (define glPixelTransferi (GL GLvoid "glPixelTransferi" GLenum GLint))
   (define glPixelZoom   (GL GLvoid "glPixelZoom" GLfloat GLfloat))
   (define glPointSize   (GL GLvoid "glPointSize" GLfloat))
   (define glPolygonStipple (GL GLvoid "glPolygonStipple" GLubyte*))
   (define glPolygonMode (GL GLvoid "glPolygonMode" GLenum GLenum))

   (define glPixelStoref (GL GLvoid "glPixelStoref" GLenum GLfloat))
   (define glPixelStorei (GL GLvoid "glPixelStorei" GLenum GLint))

   (define glPopAttrib   (GL GLvoid "glPopAttrib"))
   (define glPopMatrix   (GL GLvoid "glPopMatrix"))
   (define glPopName     (GL GLvoid "glPopName"))
   (define glPushAttrib  (GL GLvoid "glPushAttrib" GLbitfield))
   (define glPushMatrix  (GL GLvoid "glPushMatrix"))
   (define glPushName    (GL GLvoid "glPushName" GLuint))

   (define glRasterPos2d (GL GLvoid "glRasterPos2d" GLdouble GLdouble))
   (define glRasterPos2dv(GL GLvoid "glRasterPos2dv" GLdouble*))
   (define glRasterPos2f (GL GLvoid "glRasterPos2f" GLfloat GLfloat))
   (define glRasterPos2fv(GL GLvoid "glRasterPos2fv" GLfloat*))
   (define glRasterPos2i (GL GLvoid "glRasterPos2i" GLint GLint))
   (define glRasterPos2iv(GL GLvoid "glRasterPos2iv" GLint*))
   (define glRasterPos2s (GL GLvoid "glRasterPos2s" GLshort GLshort))
   (define glRasterPos2sv(GL GLvoid "glRasterPos2sv" GLshort*))
   (define glRasterPos3d (GL GLvoid "glRasterPos3d" GLdouble GLdouble GLdouble))
   (define glRasterPos3dv(GL GLvoid "glRasterPos3dv" GLdouble*))
   (define glRasterPos3f (GL GLvoid "glRasterPos3f" GLfloat GLfloat GLfloat))
   (define glRasterPos3fv(GL GLvoid "glRasterPos3fv" GLfloat*))
   (define glRasterPos3i (GL GLvoid "glRasterPos3i" GLint GLint GLint))
   (define glRasterPos3iv(GL GLvoid "glRasterPos3iv" GLint*))
   (define glRasterPos3s (GL GLvoid "glRasterPos3s" GLshort GLshort GLshort))
   (define glRasterPos3sv(GL GLvoid "glRasterPos3sv" GLshort*))
   (define glRasterPos4d (GL GLvoid "glRasterPos4d" GLdouble GLdouble GLdouble GLdouble))
   (define glRasterPos4dv(GL GLvoid "glRasterPos4dv" GLdouble*))
   (define glRasterPos4f (GL GLvoid "glRasterPos4f" GLfloat GLfloat GLfloat GLfloat))
   (define glRasterPos4fv(GL GLvoid "glRasterPos4fv" GLfloat*))
   (define glRasterPos4i (GL GLvoid "glRasterPos4i" GLint GLint GLint GLint))
   (define glRasterPos4iv(GL GLvoid "glRasterPos4iv" GLint*))
   (define glRasterPos4s (GL GLvoid "glRasterPos4s" GLshort GLshort GLshort GLshort))
   (define glRasterPos4sv(GL GLvoid "glRasterPos4sv" GLshort*))

   (define glReadBuffer  (GL GLvoid "glReadBuffer" GLenum))
   (define glReadPixels  (GL GLvoid "glReadPixels" GLint GLint GLsizei GLsizei GLenum GLenum GLvoid*))

   (define glRectd       (GL GLvoid "glRectd"  GLdouble GLdouble GLdouble GLdouble))
   (define glRectdv      (GL GLvoid "glRectdv" GLdouble* GLdouble*))
   (define glRectf       (GL GLvoid "glRectf"  GLfloat GLfloat GLfloat GLfloat))
   (define glRectfv      (GL GLvoid "glRectfv" GLfloat* GLfloat*))
   (define glRecti       (GL GLvoid "glRecti"  GLint GLint GLint GLint))
   (define glRectiv      (GL GLvoid "glRectiv" GLint* GLint*))
   (define glRects       (GL GLvoid "glRects"  GLshort GLshort GLshort GLshort))
   (define glRectsv      (GL GLvoid "glRectsv" GLshort* GLshort*))
   (define glRenderMode  (GL GLint "glRenderMode" GLenum))
   (define glRotated     (GL GLvoid "glRotated" GLdouble GLdouble GLdouble GLdouble))
   (define glRotatef     (GL GLvoid "glRotatef" GLfloat GLfloat GLfloat GLfloat))
   (define glScaled      (GL GLvoid "glScaled" GLdouble GLdouble GLdouble))
   (define glScalef      (GL GLvoid "glScalef" GLfloat GLfloat GLfloat))
   (define glScissor     (GL GLvoid "glScissor" GLint GLint GLsizei GLsizei))
   (define glSelectBuffer(GL GLvoid "glSelectBuffer" GLsizei GLuint*))
   (define glShadeModel  (GL GLvoid "glShadeModel" GLenum))
   (define glStencilFunc (GL GLvoid "glStencilFunc" GLenum GLint GLuint))
   (define glStencilMask (GL GLvoid "glStencilMask" GLuint))
   (define glStencilOp   (GL GLvoid "glStencilOp" GLenum GLenum GLenum))
   (define glTexCoord1d  (GL GLvoid "glTexCoord1d" GLdouble))
   (define glTexCoord1dv (GL GLvoid "glTexCoord1dv" GLdouble*))
   (define glTexCoord1f  (GL GLvoid "glTexCoord1f" GLfloat))
   (define glTexCoord1fv (GL GLvoid "glTexCoord1fv" GLfloat*))
   (define glTexCoord1i  (GL GLvoid "glTexCoord1i" GLint))
   (define glTexCoord1iv (GL GLvoid "glTexCoord1iv" GLint*))
   (define glTexCoord1s  (GL GLvoid "glTexCoord1s" GLshort))
   (define glTexCoord1sv (GL GLvoid "glTexCoord1sv" GLshort*))
   (define glTexCoord2d  (GL GLvoid "glTexCoord2d" GLdouble GLdouble))
   (define glTexCoord2dv (GL GLvoid "glTexCoord2dv" GLdouble*))
   (define glTexCoord2f  (GL GLvoid "glTexCoord2f" GLfloat GLfloat))
   (define glTexCoord2fv (GL GLvoid "glTexCoord2fv" GLfloat*))
   (define glTexCoord2i  (GL GLvoid "glTexCoord2i" GLint GLint))
   (define glTexCoord2iv (GL GLvoid "glTexCoord2iv" GLint*))
   (define glTexCoord2s  (GL GLvoid "glTexCoord2s" GLshort GLshort))
   (define glTexCoord2sv (GL GLvoid "glTexCoord2sv" GLshort*))
   (define glTexCoord3d  (GL GLvoid "glTexCoord3d" GLdouble GLdouble GLdouble))
   (define glTexCoord3dv (GL GLvoid "glTexCoord3dv" GLdouble*))
   (define glTexCoord3f  (GL GLvoid "glTexCoord3f" GLfloat GLfloat GLfloat))
   (define glTexCoord3fv (GL GLvoid "glTexCoord3fv" GLfloat*))
   (define glTexCoord3i  (GL GLvoid "glTexCoord3i" GLint GLint GLint))
   (define glTexCoord3iv (GL GLvoid "glTexCoord3iv" GLint*))
   (define glTexCoord3s  (GL GLvoid "glTexCoord3s" GLshort GLshort GLshort))
   (define glTexCoord3sv (GL GLvoid "glTexCoord3sv" GLshort*))
   (define glTexCoord4d  (GL GLvoid "glTexCoord4d" GLdouble GLdouble GLdouble GLdouble))
   (define glTexCoord4dv (GL GLvoid "glTexCoord4dv" GLdouble*))
   (define glTexCoord4f  (GL GLvoid "glTexCoord4f" GLfloat GLfloat GLfloat GLfloat))
   (define glTexCoord4fv (GL GLvoid "glTexCoord4fv" GLfloat*))
   (define glTexCoord4i  (GL GLvoid "glTexCoord4i" GLint GLint GLint GLint))
   (define glTexCoord4iv (GL GLvoid "glTexCoord4iv" GLint*))
   (define glTexCoord4s  (GL GLvoid "glTexCoord4s" GLshort GLshort GLshort GLshort))
   (define glTexCoord4sv (GL GLvoid "glTexCoord4sv" GLshort*))
   (define glTexEnvf     (GL GLvoid "glTexEnvf" GLenum GLenum GLfloat))
   (define glTexEnvfv    (GL GLvoid "glTexEnvfv" GLenum GLenum GLfloat*))
   (define glTexEnvi     (GL GLvoid "glTexEnvi" GLenum GLenum GLint))
   (define glTexEnviv    (GL GLvoid "glTexEnviv" GLenum GLenum GLint*))
   (define glTexGend     (GL GLvoid "glTexGend" GLenum GLenum GLdouble))
   (define glTexGendv    (GL GLvoid "glTexGendv" GLenum GLenum GLdouble*))
   (define glTexGenf     (GL GLvoid "glTexGenf" GLenum GLenum GLfloat))
   (define glTexGenfv    (GL GLvoid "glTexGenfv" GLenum GLenum GLfloat*))
   (define glTexGeni     (GL GLvoid "glTexGeni" GLenum GLenum GLint))
   (define glTexGeniv    (GL GLvoid "glTexGeniv" GLenum GLenum GLint*))
   (define glTexImage1D  (GL GLvoid "glTexImage1D" GLenum GLint GLint GLsizei GLint GLenum GLenum fft-any))
   (define glTexImage2D  (GL GLvoid "glTexImage2D" GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum fft-any))
   (define glTexParameterf (GL GLvoid "glTexParameterf" GLenum GLenum GLfloat))
   (define glTexParameterfv (GL GLvoid "glTexParameterfv" GLenum GLenum GLfloat*))
   (define glTexParameteri (GL GLvoid "glTexParameteri" GLenum GLenum GLint))
   (define glTexParameteriv (GL GLvoid "glTexParameteriv" GLenum GLenum GLint*))

   (define glTranslated  (GL GLvoid "glTranslated" GLdouble GLdouble GLdouble))
   (define glTranslatef  (GL GLvoid "glTranslatef" GLfloat GLfloat GLfloat))

   (define glVertex2d    (GL GLvoid "glVertex2d"  GLdouble GLdouble))
   (define glVertex2dv   (GL GLvoid "glVertex2dv" GLdouble*))
   (define glVertex2f    (GL GLvoid "glVertex2f"  GLfloat GLfloat))
   (define glVertex2fv   (GL GLvoid "glVertex2fv" GLfloat*))
   (define glVertex2i    (GL GLvoid "glVertex2i"  GLint GLint))
   (define glVertex2iv   (GL GLvoid "glVertex2iv" GLint*))
   (define glVertex2s    (GL GLvoid "glVertex2s"  GLshort GLshort))
   (define glVertex2sv   (GL GLvoid "glVertex2sv" GLshort*))
   (define glVertex3d    (GL GLvoid "glVertex3d"  GLdouble GLdouble GLdouble))
   (define glVertex3dv   (GL GLvoid "glVertex3dv" GLdouble*))
   (define glVertex3f    (GL GLvoid "glVertex3f"  GLfloat GLfloat GLfloat))
   (define glVertex3fv   (GL GLvoid "glVertex3fv" GLfloat*))
   (define glVertex3i    (GL GLvoid "glVertex3i"  GLint GLint GLint))
   (define glVertex3iv   (GL GLvoid "glVertex3iv" GLint*))
   (define glVertex3s    (GL GLvoid "glVertex3s"  GLshort GLshort GLshort))
   (define glVertex3sv   (GL GLvoid "glVertex3sv" GLshort*))
   (define glVertex4d    (GL GLvoid "glVertex4d"  GLdouble GLdouble GLdouble GLdouble))
   (define glVertex4dv   (GL GLvoid "glVertex4dv" GLdouble*))
   (define glVertex4f    (GL GLvoid "glVertex4f"  GLfloat GLfloat GLfloat GLfloat))
   (define glVertex4fv   (GL GLvoid "glVertex4fv" GLfloat*))
   (define glVertex4i    (GL GLvoid "glVertex4i"  GLint GLint GLint GLint))
   (define glVertex4iv   (GL GLvoid "glVertex4iv" GLint*))
   (define glVertex4s    (GL GLvoid "glVertex4s"  GLshort GLshort GLshort GLshort))
   (define glVertex4sv   (GL GLvoid "glVertex4sv" GLshort*))

   (define glViewport    (GL GLvoid "glViewport" GLint GLint GLsizei GLsizei))
)

; ==========================================================================
; GLU
(cond-expand
   (Linux
      (begin
         (define GLU_LIBRARY (or
            (load-dynamic-library "libGLU.so")
            (load-dynamic-library "libGLU.so.1")))))
   (Windows
      (begin
         (define GLU_LIBRARY (load-dynamic-library "glu32.dll"))))
   (Android
      (begin
         (define GLU_LIBRARY (load-dynamic-library "libGLU.so"))))
   (Darwin
      (begin
         (define GLU_LIBRARY (load-dynamic-library "/System/Library/Frameworks/OpenGL.framework/Libraries/libGLU.dylib"))))

;;       ;"HP-UX"
;;       ;"SunOS"
;;       ;"FreeBSD"
;;       ;"CYGWIN_NT-5.2-WOW64"
;;       ;"MINGW32_NT-5.2"
;;    (runtime-error "Can't find glu library" GLU_LIBRARY)))
)

(begin

   (define GLU_VERSION_1_0 1)
   (define GLU_VERSION_1_1 1)

   (define GLUquadric* type-vptr)

   (setq GLU GLU_LIBRARY)
   (define gluErrorString (GLU GLubyte* "gluErrorString" GLenum))
   (define gluOrtho2D     (GLU GLvoid   "gluOrtho2D"     GLdouble GLdouble GLdouble GLdouble))
   (define gluPerspective (GLU GLvoid   "gluPerspective" GLdouble GLdouble GLdouble GLdouble))
   (define gluLookAt      (GLU GLvoid   "gluLookAt"      GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble))

   (define gluNewQuadric    (GLU GLUquadric* "gluNewQuadric"))
   (define gluDeleteQuadric (GLU GLvoid "gluDeleteQuadric" GLUquadric*))
   (define gluQuadricDrawStyle (GLU GLvoid "gluQuadricDrawStyle" GLUquadric* GLenum))
      (define GLU_POINT               100010)
      (define GLU_LINE                100011)
      (define GLU_FILL                100012)
      (define GLU_SILHOUETTE          100013)
   (define gluQuadricOrientation (GLU GLvoid "gluQuadricOrientation" GLUquadric* GLenum))
      (define GLU_OUTSIDE             100020)
      (define GLU_INSIDE              100021)

   (define gluSphere (GLU GLvoid "gluSphere" GLUquadric* GLdouble GLint GLint))
   (define gluCylinder (GLU GLvoid "gluCylinder" GLUquadric* GLdouble GLdouble GLdouble GLint GLint))


   (define GLUnurbs* type-vptr)

   (define gluNewNurbsRenderer (GLU GLUnurbs* "gluNewNurbsRenderer"))
   (define gluBeginSurface (GLU GLvoid "gluBeginSurface" GLUnurbs*))
   (define gluNurbsSurface (GLU GLvoid "gluNurbsSurface" GLUnurbs* GLint GLfloat* GLint GLfloat* GLint GLint GLfloat* GLint GLint GLenum))
   (define gluEndSurface   (GLU GLvoid "gluEndSurface" GLUnurbs*))
   (define gluNurbsProperty(GLU GLvoid "gluNurbsProperty" GLUnurbs* GLenum GLfloat))
      ;       GLU_FILL                100012
      (define GLU_OUTLINE_POLYGON     100240)
      (define GLU_OUTLINE_PATCH       100241)

   (define gluBuild2DMipmaps (GLU GLint "gluBuild2DMipmaps" GLenum GLint GLsizei GLsizei GLenum GLenum GLvoid*))

(define GLU_AUTO_LOAD_MATRIX    100200)
(define GLU_CULLING             100201)
(define GLU_SAMPLING_TOLERANCE  100203)
(define GLU_DISPLAY_MODE        100204)
(define GLU_PARAMETRIC_TOLERANCE        100202)
(define GLU_SAMPLING_METHOD             100205)
(define GLU_U_STEP                      100206)
(define GLU_V_STEP                      100207)

))
