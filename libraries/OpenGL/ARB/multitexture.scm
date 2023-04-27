; ===========================================================================
; ARB_multitexture                                   (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_multitexture.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB multitexture)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies

; ---------------------------------------------------------------------------
(export ARB_multitexture

; ---------------------------------------------------------------------------
; New Procedures and Functions

	glActiveTextureARB ; void GLenum texture)
	glClientActiveTextureARB ; void GLenum texture)

	glMultiTexCoord1dARB ; void GLenum GLdouble s)
	glMultiTexCoord1dvARB ; void GLenum GLdouble*)
	glMultiTexCoord1fARB ; void GLenum GLfloat s)
	glMultiTexCoord1fvARB ; void GLenum GLfloat*)
	glMultiTexCoord1iARB ; void GLenum GLint s)
	glMultiTexCoord1ivARB ; void GLenum GLint*)
	glMultiTexCoord1sARB ; void GLenum GLshort s)
	glMultiTexCoord1svARB ; void GLenum GLshort*)
	glMultiTexCoord2dARB ; void GLenum GLdouble GLdouble t)
	glMultiTexCoord2dvARB ; void GLenum GLdouble*)
	glMultiTexCoord2fARB ; void GLenum GLfloat GLfloat t)
	glMultiTexCoord2fvARB ; void GLenum GLfloat*)
	glMultiTexCoord2iARB ; void GLenum GLint GLint t)
	glMultiTexCoord2ivARB ; void GLenum GLint*)
	glMultiTexCoord2sARB ; void GLenum GLshort GLshort t)
	glMultiTexCoord2svARB ; void GLenum GLshort*)
	glMultiTexCoord3dARB ; void GLenum GLdouble GLdouble GLdouble))
	glMultiTexCoord3dvARB ; void GLenum GLdouble*)
	glMultiTexCoord3fARB ; void GLenum GLfloat GLfloat GLfloat))
	glMultiTexCoord3fvARB ; void GLenum GLfloat*)
	glMultiTexCoord3iARB ; void GLenum GLint GLint GLint))
	glMultiTexCoord3ivARB ; void GLenum GLint*)
	glMultiTexCoord3sARB ; void GLenum GLshort GLshort GLshort))
	glMultiTexCoord3svARB ; void GLenum GLshort*)
	glMultiTexCoord4dARB ; void GLenum GLdouble GLdouble GLdouble GLdouble
	glMultiTexCoord4dvARB ; void GLenum GLdouble*)
	glMultiTexCoord4fARB ; void GLenum GLfloat GLfloat GLfloat GLfloat
	glMultiTexCoord4fvARB ; void GLenum GLfloat*)
	glMultiTexCoord4iARB ; void GLenum GLint GLint GLint GLint
	glMultiTexCoord4ivARB ; void GLenum GLint*)
	glMultiTexCoord4sARB ; void GLenum GLshort GLshort GLshort GLshort
	glMultiTexCoord4svARB ; void GLenum GLshort*)

; ---------------------------------------------------------------------------
; New Tokens

	GL_ACTIVE_TEXTURE_ARB
	GL_CLIENT_ACTIVE_TEXTURE_ARB
	GL_MAX_TEXTURE_UNITS_ARB

	GL_TEXTURE0_ARB
	GL_TEXTURE1_ARB
	GL_TEXTURE2_ARB
	GL_TEXTURE3_ARB
	GL_TEXTURE4_ARB
	GL_TEXTURE5_ARB
	GL_TEXTURE6_ARB
	GL_TEXTURE7_ARB
	GL_TEXTURE8_ARB
	GL_TEXTURE9_ARB
	GL_TEXTURE10_ARB
	GL_TEXTURE11_ARB
	GL_TEXTURE12_ARB
	GL_TEXTURE13_ARB
	GL_TEXTURE14_ARB
	GL_TEXTURE15_ARB
	GL_TEXTURE16_ARB
	GL_TEXTURE17_ARB
	GL_TEXTURE18_ARB
	GL_TEXTURE19_ARB
	GL_TEXTURE20_ARB
	GL_TEXTURE21_ARB
	GL_TEXTURE22_ARB
	GL_TEXTURE23_ARB
	GL_TEXTURE24_ARB
	GL_TEXTURE25_ARB
	GL_TEXTURE26_ARB
	GL_TEXTURE27_ARB
	GL_TEXTURE28_ARB
	GL_TEXTURE29_ARB
	GL_TEXTURE30_ARB
	GL_TEXTURE31_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_multitexture (gl:QueryExtension "GL_ARB_multitexture"))

   (setq GL gl:GetProcAddress)
	(define glActiveTextureARB (GL GLvoid "glActiveTextureARB" GLenum))
	(define glClientActiveTextureARB (GL GLvoid "glClientActiveTextureARB" GLenum))
	(define glMultiTexCoord1dARB (GL GLvoid "glMultiTexCoord1dARB" GLenum GLdouble))
	(define glMultiTexCoord1dvARB (GL GLvoid "glMultiTexCoord1dvARB" GLenum GLdouble*))
	(define glMultiTexCoord1fARB (GL GLvoid "glMultiTexCoord1fARB" GLenum GLfloat))
	(define glMultiTexCoord1fvARB (GL GLvoid "glMultiTexCoord1fvARB" GLenum GLfloat*))
	(define glMultiTexCoord1iARB (GL GLvoid "glMultiTexCoord1iARB" GLenum GLint))
	(define glMultiTexCoord1ivARB (GL GLvoid "glMultiTexCoord1ivARB" GLenum GLint*))
	(define glMultiTexCoord1sARB (GL GLvoid "glMultiTexCoord1sARB" GLenum GLshort))
	(define glMultiTexCoord1svARB (GL GLvoid "glMultiTexCoord1svARB" GLenum GLshort*))
	(define glMultiTexCoord2dARB (GL GLvoid "glMultiTexCoord2dARB" GLenum GLdouble GLdouble))
	(define glMultiTexCoord2dvARB (GL GLvoid "glMultiTexCoord2dvARB" GLenum GLdouble*))
	(define glMultiTexCoord2fARB (GL GLvoid "glMultiTexCoord2fARB" GLenum GLfloat GLfloat))
	(define glMultiTexCoord2fvARB (GL GLvoid "glMultiTexCoord2fvARB" GLenum GLfloat*))
	(define glMultiTexCoord2iARB (GL GLvoid "glMultiTexCoord2iARB" GLenum GLint GLint))
	(define glMultiTexCoord2ivARB (GL GLvoid "glMultiTexCoord2ivARB" GLenum GLint*))
	(define glMultiTexCoord2sARB (GL GLvoid "glMultiTexCoord2sARB" GLenum GLshort GLshort))
	(define glMultiTexCoord2svARB (GL GLvoid "glMultiTexCoord2svARB" GLenum GLshort*))
	(define glMultiTexCoord3dARB (GL GLvoid "glMultiTexCoord3dARB" GLenum GLdouble GLdouble GLdouble))
	(define glMultiTexCoord3dvARB (GL GLvoid "glMultiTexCoord3dvARB" GLenum GLdouble*))
	(define glMultiTexCoord3fARB (GL GLvoid "glMultiTexCoord3fARB" GLenum GLfloat GLfloat GLfloat))
	(define glMultiTexCoord3fvARB (GL GLvoid "glMultiTexCoord3fvARB" GLenum GLfloat*))
	(define glMultiTexCoord3iARB (GL GLvoid "glMultiTexCoord3iARB" GLenum GLint GLint GLint))
	(define glMultiTexCoord3ivARB (GL GLvoid "glMultiTexCoord3ivARB" GLenum GLint*))
	(define glMultiTexCoord3sARB (GL GLvoid "glMultiTexCoord3sARB" GLenum GLshort GLshort GLshort))
	(define glMultiTexCoord3svARB (GL GLvoid "glMultiTexCoord3svARB" GLenum GLshort*))
	(define glMultiTexCoord4dARB (GL GLvoid "glMultiTexCoord4dARB" GLenum GLdouble GLdouble GLdouble GLdouble))
	(define glMultiTexCoord4dvARB (GL GLvoid "glMultiTexCoord4dvARB" GLenum GLdouble*))
	(define glMultiTexCoord4fARB (GL GLvoid "glMultiTexCoord4fARB" GLenum GLfloat GLfloat GLfloat GLfloat))
	(define glMultiTexCoord4fvARB (GL GLvoid "glMultiTexCoord4fvARB" GLenum GLfloat*))
	(define glMultiTexCoord4iARB (GL GLvoid "glMultiTexCoord4iARB" GLenum GLint GLint GLint GLint))
	(define glMultiTexCoord4ivARB (GL GLvoid "glMultiTexCoord4ivARB" GLenum GLint*))
	(define glMultiTexCoord4sARB (GL GLvoid "glMultiTexCoord4sARB" GLenum GLshort GLshort GLshort GLshort))
	(define glMultiTexCoord4svARB (GL GLvoid "glMultiTexCoord4svARB" GLenum GLshort*))

	(define GL_ACTIVE_TEXTURE_ARB #x84E0)
	(define GL_CLIENT_ACTIVE_TEXTURE_ARB #x84E1)
	(define GL_MAX_TEXTURE_UNITS_ARB #x84E2)
	(define GL_TEXTURE0_ARB  #x84C0)
	(define GL_TEXTURE1_ARB  #x84C1)
	(define GL_TEXTURE2_ARB  #x84C2)
	(define GL_TEXTURE3_ARB  #x84C3)
	(define GL_TEXTURE4_ARB  #x84C4)
	(define GL_TEXTURE5_ARB  #x84C5)
	(define GL_TEXTURE6_ARB  #x84C6)
	(define GL_TEXTURE7_ARB  #x84C7)
	(define GL_TEXTURE8_ARB  #x84C8)
	(define GL_TEXTURE9_ARB  #x84C9)
	(define GL_TEXTURE10_ARB #x84CA)
	(define GL_TEXTURE11_ARB #x84CB)
	(define GL_TEXTURE12_ARB #x84CC)
	(define GL_TEXTURE13_ARB #x84CD)
	(define GL_TEXTURE14_ARB #x84CE)
	(define GL_TEXTURE15_ARB #x84CF)
	(define GL_TEXTURE16_ARB #x84D0)
	(define GL_TEXTURE17_ARB #x84D1)
	(define GL_TEXTURE18_ARB #x84D2)
	(define GL_TEXTURE19_ARB #x84D3)
	(define GL_TEXTURE20_ARB #x84D4)
	(define GL_TEXTURE21_ARB #x84D5)
	(define GL_TEXTURE22_ARB #x84D6)
	(define GL_TEXTURE23_ARB #x84D7)
	(define GL_TEXTURE24_ARB #x84D8)
	(define GL_TEXTURE25_ARB #x84D9)
	(define GL_TEXTURE26_ARB #x84DA)
	(define GL_TEXTURE27_ARB #x84DB)
	(define GL_TEXTURE28_ARB #x84DC)
	(define GL_TEXTURE29_ARB #x84DD)
	(define GL_TEXTURE30_ARB #x84DE)
	(define GL_TEXTURE31_ARB #x84DF)

))
