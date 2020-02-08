; OpenGL 1.4 (24 Jul 2002)

(define-library (OpenGL version-1-4)
(export

   GL_VERSION_1_4

   glWindowPos2iv

;; #define GL_BLEND_DST_RGB                  0x80C8
;; #define GL_BLEND_SRC_RGB                  0x80C9
;; #define GL_BLEND_DST_ALPHA                0x80CA
;; #define GL_BLEND_SRC_ALPHA                0x80CB
;; #define GL_POINT_FADE_THRESHOLD_SIZE      0x8128
;; #define GL_DEPTH_COMPONENT16              0x81A5
   GL_DEPTH_COMPONENT24
;; #define GL_DEPTH_COMPONENT32              0x81A7
;; #define GL_MIRRORED_REPEAT                0x8370
;; #define GL_MAX_TEXTURE_LOD_BIAS           0x84FD
;; #define GL_TEXTURE_LOD_BIAS               0x8501
;; #define GL_INCR_WRAP                      0x8507
;; #define GL_DECR_WRAP                      0x8508
;; #define GL_TEXTURE_DEPTH_SIZE             0x884A
;; #define GL_TEXTURE_COMPARE_MODE           0x884C
;; #define GL_TEXTURE_COMPARE_FUNC           0x884D
;; #define GL_POINT_SIZE_MIN                 0x8126
;; #define GL_POINT_SIZE_MAX                 0x8127
;; #define GL_POINT_DISTANCE_ATTENUATION     0x8129
;; #define GL_GENERATE_MIPMAP                0x8191
;; #define GL_GENERATE_MIPMAP_HINT           0x8192
;; #define GL_FOG_COORDINATE_SOURCE          0x8450
;; #define GL_FOG_COORDINATE                 0x8451
;; #define GL_FRAGMENT_DEPTH                 0x8452
;; #define GL_CURRENT_FOG_COORDINATE         0x8453
;; #define GL_FOG_COORDINATE_ARRAY_TYPE      0x8454
;; #define GL_FOG_COORDINATE_ARRAY_STRIDE    0x8455
;; #define GL_FOG_COORDINATE_ARRAY_POINTER   0x8456
;; #define GL_FOG_COORDINATE_ARRAY           0x8457
;; #define GL_COLOR_SUM                      0x8458
;; #define GL_CURRENT_SECONDARY_COLOR        0x8459
;; #define GL_SECONDARY_COLOR_ARRAY_SIZE     0x845A
;; #define GL_SECONDARY_COLOR_ARRAY_TYPE     0x845B
;; #define GL_SECONDARY_COLOR_ARRAY_STRIDE   0x845C
;; #define GL_SECONDARY_COLOR_ARRAY_POINTER  0x845D
;; #define GL_SECONDARY_COLOR_ARRAY          0x845E
;; #define GL_TEXTURE_FILTER_CONTROL         0x8500
;; #define GL_DEPTH_TEXTURE_MODE             0x884B
;; #define GL_COMPARE_R_TO_TEXTURE           0x884E
;; #define GL_BLEND_COLOR                    0x8005
;; #define GL_BLEND_EQUATION                 0x8009
;; #define GL_CONSTANT_COLOR                 0x8001
;; #define GL_ONE_MINUS_CONSTANT_COLOR       0x8002
;; #define GL_CONSTANT_ALPHA                 0x8003
;; #define GL_ONE_MINUS_CONSTANT_ALPHA       0x8004
;; #define GL_FUNC_ADD                       0x8006
;; #define GL_FUNC_REVERSE_SUBTRACT          0x800B
;; #define GL_FUNC_SUBTRACT                  0x800A
;; #define GL_MIN                            0x8007
;; #define GL_MAX                            0x8008

;; GLAPI void APIENTRY glBlendFuncSeparate (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
;; GLAPI void APIENTRY glMultiDrawArrays (GLenum mode, const GLint *first, const GLsizei *count, GLsizei drawcount);
;; GLAPI void APIENTRY glMultiDrawElements (GLenum mode, const GLsizei *count, GLenum type, const void *const*indices, GLsizei drawcount);
;; GLAPI void APIENTRY glPointParameterf (GLenum pname, GLfloat param);
;; GLAPI void APIENTRY glPointParameterfv (GLenum pname, const GLfloat *params);
;; GLAPI void APIENTRY glPointParameteri (GLenum pname, GLint param);
;; GLAPI void APIENTRY glPointParameteriv (GLenum pname, const GLint *params);
;; GLAPI void APIENTRY glFogCoordf (GLfloat coord);
;; GLAPI void APIENTRY glFogCoordfv (const GLfloat *coord);
;; GLAPI void APIENTRY glFogCoordd (GLdouble coord);
;; GLAPI void APIENTRY glFogCoorddv (const GLdouble *coord);
;; GLAPI void APIENTRY glFogCoordPointer (GLenum type, GLsizei stride, const void *pointer);
;; GLAPI void APIENTRY glSecondaryColor3b (GLbyte red, GLbyte green, GLbyte blue);
;; GLAPI void APIENTRY glSecondaryColor3bv (const GLbyte *v);
;; GLAPI void APIENTRY glSecondaryColor3d (GLdouble red, GLdouble green, GLdouble blue);
;; GLAPI void APIENTRY glSecondaryColor3dv (const GLdouble *v);
;; GLAPI void APIENTRY glSecondaryColor3f (GLfloat red, GLfloat green, GLfloat blue);
;; GLAPI void APIENTRY glSecondaryColor3fv (const GLfloat *v);
;; GLAPI void APIENTRY glSecondaryColor3i (GLint red, GLint green, GLint blue);
;; GLAPI void APIENTRY glSecondaryColor3iv (const GLint *v);
;; GLAPI void APIENTRY glSecondaryColor3s (GLshort red, GLshort green, GLshort blue);
;; GLAPI void APIENTRY glSecondaryColor3sv (const GLshort *v);
;; GLAPI void APIENTRY glSecondaryColor3ub (GLubyte red, GLubyte green, GLubyte blue);
;; GLAPI void APIENTRY glSecondaryColor3ubv (const GLubyte *v);
;; GLAPI void APIENTRY glSecondaryColor3ui (GLuint red, GLuint green, GLuint blue);
;; GLAPI void APIENTRY glSecondaryColor3uiv (const GLuint *v);
;; GLAPI void APIENTRY glSecondaryColor3us (GLushort red, GLushort green, GLushort blue);
;; GLAPI void APIENTRY glSecondaryColor3usv (const GLushort *v);
;; GLAPI void APIENTRY glSecondaryColorPointer (GLint size, GLenum type, GLsizei stride, const void *pointer);
;; GLAPI void APIENTRY glWindowPos2d (GLdouble x, GLdouble y);
;; GLAPI void APIENTRY glWindowPos2dv (const GLdouble *v);
;; GLAPI void APIENTRY glWindowPos2f (GLfloat x, GLfloat y);
;; GLAPI void APIENTRY glWindowPos2fv (const GLfloat *v);
;; GLAPI void APIENTRY glWindowPos2i (GLint x, GLint y);
;; GLAPI void APIENTRY glWindowPos2iv (const GLint *v);
;; GLAPI void APIENTRY glWindowPos2s (GLshort x, GLshort y);
;; GLAPI void APIENTRY glWindowPos2sv (const GLshort *v);
;; GLAPI void APIENTRY glWindowPos3d (GLdouble x, GLdouble y, GLdouble z);
;; GLAPI void APIENTRY glWindowPos3dv (const GLdouble *v);
;; GLAPI void APIENTRY glWindowPos3f (GLfloat x, GLfloat y, GLfloat z);
;; GLAPI void APIENTRY glWindowPos3fv (const GLfloat *v);
;; GLAPI void APIENTRY glWindowPos3i (GLint x, GLint y, GLint z);
;; GLAPI void APIENTRY glWindowPos3iv (const GLint *v);
;; GLAPI void APIENTRY glWindowPos3s (GLshort x, GLshort y, GLshort z);
;; GLAPI void APIENTRY glWindowPos3sv (const GLshort *v);
;; GLAPI void APIENTRY glBlendColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
;; GLAPI void APIENTRY glBlendEquation (GLenum mode);

   (exports (OpenGL version-1-3)))

(import (scheme core)
   (OpenGL version-1-3))

(begin
   (define GL_VERSION_1_4 1)

   (setq GL GL_LIBRARY)
   (define glWindowPos2iv (GL GLvoid "glWindowPos2iv" (fft* GLint)))

   (define GL_DEPTH_COMPONENT24              #x81A6)

))