; GLU 1.1 - 1.3 (18 Sep 2008)
(define-library (lib GLU)
(export
      (exports (OpenGL platform))

   ; openGL Utility
   GLU_LIBRARY

   GLU_VERSION_1_1
   GLU_VERSION_1_2
   GLU_VERSION_1_3

   ; included extensions
   GLU_EXT_object_space_tess
   GLU_EXT_nurbs_tessellator

   GLU_TRUE GLU_FALSE

   ; StringName
   GLU_VERSION
   GLU_EXTENSIONS

   ; ErrorCode
   GLU_INVALID_ENUM
   GLU_INVALID_VALUE
   GLU_OUT_OF_MEMORY
   GLU_INCOMPATIBLE_GL_VERSION
   GLU_INVALID_OPERATION

   ; NurbsDisplay */
   ;GLU_FILL
   GLU_OUTLINE_POLYGON
   GLU_OUTLINE_PATCH

   ; NurbsCallback
   GLU_NURBS_ERROR
   GLU_ERROR
   GLU_NURBS_BEGIN
   GLU_NURBS_BEGIN_EXT
   GLU_NURBS_VERTEX
   GLU_NURBS_VERTEX_EXT
   GLU_NURBS_NORMAL
   GLU_NURBS_NORMAL_EXT
   GLU_NURBS_COLOR
   GLU_NURBS_COLOR_EXT
   GLU_NURBS_TEXTURE_COORD
   GLU_NURBS_TEX_COORD_EXT
   GLU_NURBS_END
   GLU_NURBS_END_EXT
   GLU_NURBS_BEGIN_DATA
   GLU_NURBS_BEGIN_DATA_EXT
   GLU_NURBS_VERTEX_DATA
   GLU_NURBS_VERTEX_DATA_EXT
   GLU_NURBS_NORMAL_DATA
   GLU_NURBS_NORMAL_DATA_EXT
   GLU_NURBS_COLOR_DATA
   GLU_NURBS_COLOR_DATA_EXT
   GLU_NURBS_TEXTURE_COORD_DATA
   GLU_NURBS_TEX_COORD_DATA_EXT
   GLU_NURBS_END_DATA
   GLU_NURBS_END_DATA_EXT

   ; NurbsError
   GLU_NURBS_ERROR1
   GLU_NURBS_ERROR2
   GLU_NURBS_ERROR3
   GLU_NURBS_ERROR4
   GLU_NURBS_ERROR5
   GLU_NURBS_ERROR6
   GLU_NURBS_ERROR7
   GLU_NURBS_ERROR8
   GLU_NURBS_ERROR9
   GLU_NURBS_ERROR10
   GLU_NURBS_ERROR11
   GLU_NURBS_ERROR12
   GLU_NURBS_ERROR13
   GLU_NURBS_ERROR14
   GLU_NURBS_ERROR15
   GLU_NURBS_ERROR16
   GLU_NURBS_ERROR17
   GLU_NURBS_ERROR18
   GLU_NURBS_ERROR19
   GLU_NURBS_ERROR20
   GLU_NURBS_ERROR21
   GLU_NURBS_ERROR22
   GLU_NURBS_ERROR23
   GLU_NURBS_ERROR24
   GLU_NURBS_ERROR25
   GLU_NURBS_ERROR26
   GLU_NURBS_ERROR27
   GLU_NURBS_ERROR28
   GLU_NURBS_ERROR29
   GLU_NURBS_ERROR30
   GLU_NURBS_ERROR31
   GLU_NURBS_ERROR32
   GLU_NURBS_ERROR33
   GLU_NURBS_ERROR34
   GLU_NURBS_ERROR35
   GLU_NURBS_ERROR36
   GLU_NURBS_ERROR37

   ; NurbsProperty
   GLU_AUTO_LOAD_MATRIX
   GLU_CULLING
   GLU_SAMPLING_TOLERANCE
   GLU_DISPLAY_MODE
   GLU_PARAMETRIC_TOLERANCE
   GLU_SAMPLING_METHOD
   GLU_U_STEP
   GLU_V_STEP
   GLU_NURBS_MODE
   GLU_NURBS_MODE_EXT
   GLU_NURBS_TESSELLATOR
   GLU_NURBS_TESSELLATOR_EXT
   GLU_NURBS_RENDERER
   GLU_NURBS_RENDERER_EXT

   ; NurbsSampling
   GLU_OBJECT_PARAMETRIC_ERROR
   GLU_OBJECT_PARAMETRIC_ERROR_EXT
   GLU_OBJECT_PATH_LENGTH
   GLU_OBJECT_PATH_LENGTH_EXT
   GLU_PATH_LENGTH
   GLU_PARAMETRIC_ERROR
   GLU_DOMAIN_DISTANCE

   ; NurbsTrim
   GLU_MAP1_TRIM_2
   GLU_MAP1_TRIM_3

   ; QuadricDrawStyle
   GLU_POINT
   GLU_LINE
   GLU_FILL
   GLU_SILHOUETTE

   ; QuadricCallback
   ;GLU_ERROR

   ; QuadricNormal
   GLU_SMOOTH
   GLU_FLAT
   GLU_NONE

   ; QuadricOrientation
   GLU_OUTSIDE
   GLU_INSIDE

   ; TessCallback
   GLU_TESS_BEGIN
   GLU_BEGIN
   GLU_TESS_VERTEX
   GLU_VERTEX
   GLU_TESS_END
   GLU_END
   GLU_TESS_ERROR
   GLU_TESS_EDGE_FLAG
   GLU_EDGE_FLAG
   GLU_TESS_COMBINE
   GLU_TESS_BEGIN_DATA
   GLU_TESS_VERTEX_DATA
   GLU_TESS_END_DATA
   GLU_TESS_ERROR_DATA
   GLU_TESS_EDGE_FLAG_DATA
   GLU_TESS_COMBINE_DATA

   ; TessContour
   GLU_CW
   GLU_CCW
   GLU_INTERIOR
   GLU_EXTERIOR
   GLU_UNKNOWN

   ; TessProperty
   GLU_TESS_WINDING_RULE
   GLU_TESS_BOUNDARY_ONLY
   GLU_TESS_TOLERANCE

   ; TessError
   GLU_TESS_ERROR1
   GLU_TESS_ERROR2
   GLU_TESS_ERROR3
   GLU_TESS_ERROR4
   GLU_TESS_ERROR5
   GLU_TESS_ERROR6
   GLU_TESS_ERROR7
   GLU_TESS_ERROR8
   GLU_TESS_MISSING_BEGIN_POLYGON
   GLU_TESS_MISSING_BEGIN_CONTOUR
   GLU_TESS_MISSING_END_POLYGON
   GLU_TESS_MISSING_END_CONTOUR
   GLU_TESS_COORD_TOO_LARGE
   GLU_TESS_NEED_COMBINE_CALLBACK

   ; TessWinding
   GLU_TESS_WINDING_ODD
   GLU_TESS_WINDING_NONZERO
   GLU_TESS_WINDING_POSITIVE
   GLU_TESS_WINDING_NEGATIVE
   GLU_TESS_WINDING_ABS_GEQ_TWO

   gluBeginCurve ; void (GLUnurbs* nurb)
   gluBeginPolygon ; void (GLUtesselator* tess)
   gluBeginSurface ; void (GLUnurbs* nurb)
   gluBeginTrim ; void (GLUnurbs* nurb)
   gluBuild1DMipmapLevels ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)
   gluBuild1DMipmaps ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLenum format, GLenum type, const void *data)
   gluBuild2DMipmapLevels ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)
   gluBuild2DMipmaps ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *data)
   gluBuild3DMipmapLevels ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, GLint level, GLint base, GLint max, const void *data)
   gluBuild3DMipmaps ; GLint (GLenum target, GLint internalFormat, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *data)
   gluCheckExtension ; GLboolean (const GLubyte *extName, const GLubyte *extString)
   gluCylinder ; void (GLUquadric* quad, GLdouble base, GLdouble top, GLdouble height, GLint slices, GLint stacks)
   gluDeleteNurbsRenderer ; void (GLUnurbs* nurb)
   gluDeleteQuadric ; void (GLUquadric* quad)
   gluDeleteTess ; void (GLUtesselator* tess)
   gluDisk ; void (GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops)
   gluEndCurve ; void (GLUnurbs* nurb)
   gluEndPolygon ; void (GLUtesselator* tess)
   gluEndSurface ; void (GLUnurbs* nurb)
   gluEndTrim ; void (GLUnurbs* nurb)
   gluErrorString ; const GLubyte * (GLenum error)
   gluGetNurbsProperty ; void (GLUnurbs* nurb, GLenum property, GLfloat* data)
   gluGetString ; const GLubyte * (GLenum name)
   gluGetTessProperty ; void (GLUtesselator* tess, GLenum which, GLdouble* data)
   gluLoadSamplingMatrices ; void (GLUnurbs* nurb, const GLfloat *model, const GLfloat *perspective, const GLint *view)
   gluLookAt ; void (GLdouble eyeX, GLdouble eyeY, GLdouble eyeZ, GLdouble centerX, GLdouble centerY, GLdouble centerZ, GLdouble upX, GLdouble upY, GLdouble upZ)
   gluNewNurbsRenderer ; GLUnurbs* (void)
   gluNewQuadric ; GLUquadric* (void)
   gluNewTess ; GLUtesselator* (void)
   gluNextContour ; void (GLUtesselator* tess, GLenum type)
   gluNurbsCallback ; void (GLUnurbs* nurb, GLenum which, _GLUfuncptr CallBackFunc)
   gluNurbsCallbackData ; void (GLUnurbs* nurb, GLvoid* userData)
   gluNurbsCallbackDataEXT ; void (GLUnurbs* nurb, GLvoid* userData)
   gluNurbsCurve ; void (GLUnurbs* nurb, GLint knotCount, GLfloat *knots, GLint stride, GLfloat *control, GLint order, GLenum type)
   gluNurbsProperty ; void (GLUnurbs* nurb, GLenum property, GLfloat value)
   gluNurbsSurface ; void (GLUnurbs* nurb, GLint sKnotCount, GLfloat* sKnots, GLint tKnotCount, GLfloat* tKnots, GLint sStride, GLint tStride, GLfloat* control, GLint sOrder, GLint tOrder, GLenum type)
   gluOrtho2D ; void (GLdouble left, GLdouble right, GLdouble bottom, GLdouble top)
   gluPartialDisk ; void (GLUquadric* quad, GLdouble inner, GLdouble outer, GLint slices, GLint loops, GLdouble start, GLdouble sweep)
   gluPerspective ; void (GLdouble fovy, GLdouble aspect, GLdouble zNear, GLdouble zFar)
   gluPickMatrix ; void (GLdouble x, GLdouble y, GLdouble delX, GLdouble delY, GLint *viewport)
   gluProject ; GLint (GLdouble objX, GLdouble objY, GLdouble objZ, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble* winX, GLdouble* winY, GLdouble* winZ)
   gluPwlCurve ; void (GLUnurbs* nurb, GLint count, GLfloat* data, GLint stride, GLenum type)
   gluQuadricCallback ; void (GLUquadric* quad, GLenum which, _GLUfuncptr CallBackFunc)
   gluQuadricDrawStyle ; void (GLUquadric* quad, GLenum draw)
   gluQuadricNormals ; void (GLUquadric* quad, GLenum normal)
   gluQuadricOrientation ; void (GLUquadric* quad, GLenum orientation)
   gluQuadricTexture ; void (GLUquadric* quad, GLboolean texture)
   gluScaleImage ; GLint (GLenum format, GLsizei wIn, GLsizei hIn, GLenum typeIn, const void *dataIn, GLsizei wOut, GLsizei hOut, GLenum typeOut, GLvoid* dataOut)
   gluSphere ; void (GLUquadric* quad, GLdouble radius, GLint slices, GLint stacks)
   gluTessBeginContour ; void (GLUtesselator* tess)
   gluTessBeginPolygon ; void (GLUtesselator* tess, GLvoid* data)
   gluTessCallback ; void (GLUtesselator* tess, GLenum which, _GLUfuncptr CallBackFunc)
   gluTessEndContour ; void (GLUtesselator* tess)
   gluTessEndPolygon ; void (GLUtesselator* tess)
   gluTessNormal ; void (GLUtesselator* tess, GLdouble valueX, GLdouble valueY, GLdouble valueZ)
   gluTessProperty ; void (GLUtesselator* tess, GLenum which, GLdouble data)
   gluTessVertex ; void (GLUtesselator* tess, GLdouble *location, GLvoid* data)
   gluUnProject ; GLint (GLdouble winX, GLdouble winY, GLdouble winZ, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble* objX, GLdouble* objY, GLdouble* objZ)
   gluUnProject4 ; GLint (GLdouble winX, GLdouble winY, GLdouble winZ, GLdouble clipW, const GLdouble *model, const GLdouble *proj, const GLint *view, GLdouble nearVal, GLdouble farVal, GLdouble* objX, GLdouble* objY, GLdouble* objZ, GLdouble* objW)
)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL platform))

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
   (else
      (begin
         (define GLU_LIBRARY (load-dynamic-library #false)))))

(begin

   (define GLU_VERSION_1_0 1)
   (define GLU_VERSION_1_1 1)
   (define GLU_VERSION_1_2 1)
   (define GLU_VERSION_1_3 1)

   (define GLU_EXT_nurbs_tessellator 1)
   (define GLU_EXT_object_space_tess 1)

   (define GLU_TRUE  1)
   (define GLU_FALSE 0)
   
   ; -------------------------------------------------------------------------
   ; constants
   (define GLU_VERSION                        100800)
   (define GLU_EXTENSIONS                     100801)

   (define GLU_INVALID_ENUM                   100900)
   (define GLU_INVALID_VALUE                  100901)
   (define GLU_OUT_OF_MEMORY                  100902)
   (define GLU_INCOMPATIBLE_GL_VERSION        100903)
   (define GLU_INVALID_OPERATION              100904)

   (define GLU_OUTLINE_POLYGON                100240)
   (define GLU_OUTLINE_PATCH                  100241)

   (define GLU_NURBS_ERROR                    100103)
   (define GLU_ERROR                          100103)
   (define GLU_NURBS_BEGIN                    100164)
   (define GLU_NURBS_BEGIN_EXT                100164)
   (define GLU_NURBS_VERTEX                   100165)
   (define GLU_NURBS_VERTEX_EXT               100165)
   (define GLU_NURBS_NORMAL                   100166)
   (define GLU_NURBS_NORMAL_EXT               100166)
   (define GLU_NURBS_COLOR                    100167)
   (define GLU_NURBS_COLOR_EXT                100167)
   (define GLU_NURBS_TEXTURE_COORD            100168)
   (define GLU_NURBS_TEX_COORD_EXT            100168)
   (define GLU_NURBS_END                      100169)
   (define GLU_NURBS_END_EXT                  100169)
   (define GLU_NURBS_BEGIN_DATA               100170)
   (define GLU_NURBS_BEGIN_DATA_EXT           100170)
   (define GLU_NURBS_VERTEX_DATA              100171)
   (define GLU_NURBS_VERTEX_DATA_EXT          100171)
   (define GLU_NURBS_NORMAL_DATA              100172)
   (define GLU_NURBS_NORMAL_DATA_EXT          100172)
   (define GLU_NURBS_COLOR_DATA               100173)
   (define GLU_NURBS_COLOR_DATA_EXT           100173)
   (define GLU_NURBS_TEXTURE_COORD_DATA       100174)
   (define GLU_NURBS_TEX_COORD_DATA_EXT       100174)
   (define GLU_NURBS_END_DATA                 100175)
   (define GLU_NURBS_END_DATA_EXT             100175)

   (define GLU_NURBS_ERROR1                   100251)
   (define GLU_NURBS_ERROR2                   100252)
   (define GLU_NURBS_ERROR3                   100253)
   (define GLU_NURBS_ERROR4                   100254)
   (define GLU_NURBS_ERROR5                   100255)
   (define GLU_NURBS_ERROR6                   100256)
   (define GLU_NURBS_ERROR7                   100257)
   (define GLU_NURBS_ERROR8                   100258)
   (define GLU_NURBS_ERROR9                   100259)
   (define GLU_NURBS_ERROR10                  100260)
   (define GLU_NURBS_ERROR11                  100261)
   (define GLU_NURBS_ERROR12                  100262)
   (define GLU_NURBS_ERROR13                  100263)
   (define GLU_NURBS_ERROR14                  100264)
   (define GLU_NURBS_ERROR15                  100265)
   (define GLU_NURBS_ERROR16                  100266)
   (define GLU_NURBS_ERROR17                  100267)
   (define GLU_NURBS_ERROR18                  100268)
   (define GLU_NURBS_ERROR19                  100269)
   (define GLU_NURBS_ERROR20                  100270)
   (define GLU_NURBS_ERROR21                  100271)
   (define GLU_NURBS_ERROR22                  100272)
   (define GLU_NURBS_ERROR23                  100273)
   (define GLU_NURBS_ERROR24                  100274)
   (define GLU_NURBS_ERROR25                  100275)
   (define GLU_NURBS_ERROR26                  100276)
   (define GLU_NURBS_ERROR27                  100277)
   (define GLU_NURBS_ERROR28                  100278)
   (define GLU_NURBS_ERROR29                  100279)
   (define GLU_NURBS_ERROR30                  100280)
   (define GLU_NURBS_ERROR31                  100281)
   (define GLU_NURBS_ERROR32                  100282)
   (define GLU_NURBS_ERROR33                  100283)
   (define GLU_NURBS_ERROR34                  100284)
   (define GLU_NURBS_ERROR35                  100285)
   (define GLU_NURBS_ERROR36                  100286)
   (define GLU_NURBS_ERROR37                  100287)

   (define GLU_AUTO_LOAD_MATRIX               100200)
   (define GLU_CULLING                        100201)
   (define GLU_SAMPLING_TOLERANCE             100203)
   (define GLU_DISPLAY_MODE                   100204)
   (define GLU_PARAMETRIC_TOLERANCE           100202)
   (define GLU_SAMPLING_METHOD                100205)
   (define GLU_U_STEP                         100206)
   (define GLU_V_STEP                         100207)
   (define GLU_NURBS_MODE                     100160)
   (define GLU_NURBS_MODE_EXT                 100160)
   (define GLU_NURBS_TESSELLATOR              100161)
   (define GLU_NURBS_TESSELLATOR_EXT          100161)
   (define GLU_NURBS_RENDERER                 100162)
   (define GLU_NURBS_RENDERER_EXT             100162)

   (define GLU_OBJECT_PARAMETRIC_ERROR        100208)
   (define GLU_OBJECT_PARAMETRIC_ERROR_EXT    100208)
   (define GLU_OBJECT_PATH_LENGTH             100209)
   (define GLU_OBJECT_PATH_LENGTH_EXT         100209)
   (define GLU_PATH_LENGTH                    100215)
   (define GLU_PARAMETRIC_ERROR               100216)
   (define GLU_DOMAIN_DISTANCE                100217)

   (define GLU_MAP1_TRIM_2                    100210)
   (define GLU_MAP1_TRIM_3                    100211)

   (define GLU_POINT                          100010)
   (define GLU_LINE                           100011)
   (define GLU_FILL                           100012)
   (define GLU_SILHOUETTE                     100013)

   (define GLU_SMOOTH                         100000)
   (define GLU_FLAT                           100001)
   (define GLU_NONE                           100002)

   (define GLU_OUTSIDE                        100020)
   (define GLU_INSIDE                         100021)

   (define GLU_TESS_BEGIN                     100100)
   (define GLU_BEGIN                          100100)
   (define GLU_TESS_VERTEX                    100101)
   (define GLU_VERTEX                         100101)
   (define GLU_TESS_END                       100102)
   (define GLU_END                            100102)
   (define GLU_TESS_ERROR                     100103)
   (define GLU_TESS_EDGE_FLAG                 100104)
   (define GLU_EDGE_FLAG                      100104)
   (define GLU_TESS_COMBINE                   100105)
   (define GLU_TESS_BEGIN_DATA                100106)
   (define GLU_TESS_VERTEX_DATA               100107)
   (define GLU_TESS_END_DATA                  100108)
   (define GLU_TESS_ERROR_DATA                100109)
   (define GLU_TESS_EDGE_FLAG_DATA            100110)
   (define GLU_TESS_COMBINE_DATA              100111)

   (define GLU_CW                             100120)
   (define GLU_CCW                            100121)
   (define GLU_INTERIOR                       100122)
   (define GLU_EXTERIOR                       100123)
   (define GLU_UNKNOWN                        100124)

   (define GLU_TESS_WINDING_RULE              100140)
   (define GLU_TESS_BOUNDARY_ONLY             100141)
   (define GLU_TESS_TOLERANCE                 100142)

   (define GLU_TESS_ERROR1                    100151)
   (define GLU_TESS_ERROR2                    100152)
   (define GLU_TESS_ERROR3                    100153)
   (define GLU_TESS_ERROR4                    100154)
   (define GLU_TESS_ERROR5                    100155)
   (define GLU_TESS_ERROR6                    100156)
   (define GLU_TESS_ERROR7                    100157)
   (define GLU_TESS_ERROR8                    100158)
   (define GLU_TESS_MISSING_BEGIN_POLYGON     100151)
   (define GLU_TESS_MISSING_BEGIN_CONTOUR     100152)
   (define GLU_TESS_MISSING_END_POLYGON       100153)
   (define GLU_TESS_MISSING_END_CONTOUR       100154)
   (define GLU_TESS_COORD_TOO_LARGE           100155)
   (define GLU_TESS_NEED_COMBINE_CALLBACK     100156)

   (define GLU_TESS_WINDING_ODD               100130)
   (define GLU_TESS_WINDING_NONZERO           100131)
   (define GLU_TESS_WINDING_POSITIVE          100132)
   (define GLU_TESS_WINDING_NEGATIVE          100133)
   (define GLU_TESS_WINDING_ABS_GEQ_TWO       100134)

   (define GLUnurbs* type-vptr)
   (define GLUquadric* type-vptr)
   (define GLUtesselator* type-vptr)

   (define _GLUfuncptr type-callable)
   ;GLU_NURBS_BEGIN_CALLBACK
   ;GLU_NURBS_BEGIN_DATA_CALLBACK
   ;GLU_NURBS_VERTEX_CALLBACK
   ;GLU_NURBS_VERTEX_DATA_CALLBACK
   ;GLU_NURBS_NORMAL_CALLBACK
   ;GLU_NURBS_NORMAL_DATA_CALLBACK
   ;GLU_NURBS_COLOR_CALLBACK
   ;GLU_NURBS_COLOR_DATA_CALLBACK
   ;GLU_NURBS_TEXTURE_COORD_CALLBACK
   ;GLU_NURBS_TEXTURE_COORD_DATA_CALLBACK
   ;GLU_NURBS_END_CALLBACK
   ;GLU_NURBS_END_DATA_CALLBACK
   ;GLU_NURBS_ERROR_CALLBACK

   (setq GLU GLU_LIBRARY)

   ; functions
   (define gluBeginCurve   (GLU GLvoid "gluBeginCurve" GLUnurbs*))
   (define gluBeginPolygon (GLU GLvoid "gluBeginPolygon" GLUtesselator*))
   (define gluBeginSurface (GLU GLvoid "gluBeginSurface" GLUnurbs*))
   (define gluBeginTrim    (GLU GLvoid "gluBeginTrim" GLUnurbs*))

   (define gluBuild1DMipmapLevels (GLU GLint "gluBuild1DMipmapLevels" GLenum GLint GLsizei GLenum GLenum GLint GLint GLint GLvoid*))
   (define gluBuild1DMipmaps      (GLU GLint "gluBuild1DMipmaps"      GLenum GLint GLsizei GLenum GLenum GLvoid*))
   (define gluBuild2DMipmapLevels (GLU GLint "gluBuild2DMipmapLevels" GLenum GLint GLsizei GLsizei GLenum GLenum GLint GLint GLint GLvoid*))
   (define gluBuild2DMipmaps      (GLU GLint "gluBuild2DMipmaps"      GLenum GLint GLsizei GLsizei GLenum GLenum GLvoid*))
   (define gluBuild3DMipmapLevels (GLU GLint "gluBuild3DMipmapLevels" GLenum GLint GLsizei GLsizei GLsizei GLenum GLenum GLint GLint GLint GLvoid*))
   (define gluBuild3DMipmaps      (GLU GLint "gluBuild3DMipmaps"      GLenum GLint GLsizei GLsizei GLsizei GLenum GLenum GLvoid*))

   (define gluCheckExtension (GLU GLboolean "gluCheckExtension" GLubyte* GLubyte*))
   (define gluCylinder (GLU GLvoid "gluCylinder" GLUquadric* GLdouble GLdouble GLdouble GLint GLint))
   (define gluDeleteNurbsRenderer (GLU GLvoid "gluDeleteNurbsRenderer" GLUnurbs*))
   (define gluDeleteQuadric (GLU GLvoid "gluDeleteQuadric" GLUquadric*))
   (define gluDeleteTess (GLU GLvoid "gluDeleteTess" GLUtesselator*))
   (define gluDisk (GLU GLvoid "gluDisk" GLUquadric* GLdouble GLdouble GLint GLint))
   (define gluEndCurve (GLU GLvoid "gluEndCurve" GLUnurbs*))
   (define gluEndPolygon (GLU GLvoid "gluEndPolygon" GLUtesselator*))
   (define gluEndSurface (GLU GLvoid "gluEndSurface" GLUnurbs*))
   (define gluEndTrim (GLU GLvoid "gluEndTrim" GLUnurbs*))
   (define gluErrorString (GLU GLubyte* "gluErrorString" GLenum))
   (define gluGetNurbsProperty (GLU GLvoid "gluGetNurbsProperty" GLUnurbs* GLenum GLfloat*))
   (define gluGetString (GLU GLubyte* "gluGetString" GLenum))
   (define gluGetTessProperty (GLU GLvoid "gluGetTessProperty" GLUtesselator* GLenum GLdouble*))
   (define gluLoadSamplingMatrices (GLU GLvoid "gluLoadSamplingMatrices" GLUnurbs* GLfloat* GLfloat* GLint*))
   (define gluLookAt (GLU GLvoid "gluLookAt" GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble GLdouble))
   (define gluNewNurbsRenderer (GLU GLUnurbs* "gluNewNurbsRenderer"))
   (define gluNewQuadric (GLU GLUquadric* "gluNewQuadric"))
   (define gluNewTess (GLU GLUtesselator* "gluNewTess"))
   (define gluNextContour (GLU GLvoid "gluNextContour" GLUtesselator* GLenum))
   (define gluNurbsCallback (GLU GLvoid "gluNurbsCallback" GLUnurbs* GLenum _GLUfuncptr))
   (define gluNurbsCallbackData (GLU GLvoid "gluNurbsCallbackData" GLUnurbs* GLvoid*))
   (define gluNurbsCallbackDataEXT (GLU GLvoid "gluNurbsCallbackDataEXT" GLUnurbs* GLvoid*))
   (define gluNurbsCurve (GLU GLvoid "gluNurbsCurve" GLUnurbs* GLint GLfloat* GLint GLfloat* GLint GLenum))
   (define gluNurbsProperty (GLU GLvoid "gluNurbsProperty" GLUnurbs* GLenum GLfloat))
   (define gluNurbsSurface (GLU GLvoid "gluNurbsSurface" GLUnurbs* GLint GLfloat* GLint GLfloat* GLint GLint GLfloat* GLint GLint GLenum))
   (define gluOrtho2D (GLU GLvoid "gluOrtho2D" GLdouble GLdouble GLdouble GLdouble))
   (define gluPartialDisk (GLU GLvoid "gluPartialDisk" GLUquadric* GLdouble GLdouble GLint GLint GLdouble GLdouble))
   (define gluPerspective (GLU GLvoid "gluPerspective" GLdouble GLdouble GLdouble GLdouble))
   (define gluPickMatrix (GLU GLvoid "gluPickMatrix" GLdouble GLdouble GLdouble GLdouble GLint*))
   (define gluProject (GLU GLint "gluProject" GLdouble GLdouble GLdouble GLdouble* GLdouble* GLint* GLdouble* GLdouble* GLdouble*))
   (define gluPwlCurve (GLU GLvoid "gluPwlCurve" GLUnurbs* GLint GLfloat* GLint GLenum))
   (define gluQuadricCallback (GLU GLvoid "gluQuadricCallback" GLUquadric* GLenum _GLUfuncptr))
   (define gluQuadricDrawStyle (GLU GLvoid "gluQuadricDrawStyle" GLUquadric* GLenum))
   (define gluQuadricNormals (GLU GLvoid "gluQuadricNormals" GLUquadric* GLenum))
   (define gluQuadricOrientation (GLU GLvoid "gluQuadricOrientation" GLUquadric* GLenum))
   (define gluQuadricTexture (GLU GLvoid "gluQuadricTexture" GLUquadric* GLboolean))
   (define gluScaleImage (GLU GLint "gluScaleImage" GLenum GLsizei GLsizei GLenum GLvoid* GLsizei GLsizei GLenum GLvoid*))
   (define gluSphere (GLU GLvoid "gluSphere" GLUquadric* GLdouble GLint GLint))
   (define gluTessBeginContour (GLU GLvoid "gluTessBeginContour" GLUtesselator*))
   (define gluTessBeginPolygon (GLU GLvoid "gluTessBeginPolygon" GLUtesselator* GLvoid*))
   (define gluTessCallback (GLU GLvoid "gluTessCallback" GLUtesselator* GLenum _GLUfuncptr))
   (define gluTessEndContour (GLU GLvoid "gluTessEndContour" GLUtesselator*))
   (define gluTessEndPolygon (GLU GLvoid "gluTessEndPolygon" GLUtesselator*))
   (define gluTessNormal (GLU GLvoid "gluTessNormal" GLUtesselator* GLdouble GLdouble GLdouble))
   (define gluTessProperty (GLU GLvoid "gluTessProperty" GLUtesselator* GLenum GLdouble))
   (define gluTessVertex (GLU GLvoid "gluTessVertex" GLUtesselator* GLdouble* GLvoid*))
   (define gluUnProject (GLU GLint "gluUnProject" GLdouble GLdouble GLdouble GLdouble* GLdouble* GLint* GLdouble* GLdouble* GLdouble*))
   (define gluUnProject4 (GLU GLint "gluUnProject4" GLdouble GLdouble GLdouble GLdouble GLdouble* GLdouble* GLint* GLdouble GLdouble GLdouble* GLdouble* GLdouble* GLdouble*))
))
