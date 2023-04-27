; OpenGL 1.3 (14 Aug 2001)
;  + ARB texture_compression
;  + ARB texture_cube_map
;  + ARB multisample
;  + ARB multitexture
;  + ARB texture_env_add
;  + ARB texture_env_combine
;  + ARB texture_env_dot3
;  + ARB transpose_matrix

(define-library (OpenGL 1.3)
(export
      (exports (OpenGL 1.2))

   GL_VERSION_1_3

;; F.1 ARB_texture_compression
   GL_COMPRESSED_ALPHA
   GL_COMPRESSED_LUMINANCE
   GL_COMPRESSED_LUMINANCE_ALPHA
   GL_COMPRESSED_INTENSITY
   GL_COMPRESSED_RGB
   GL_COMPRESSED_RGBA
   GL_TEXTURE_COMPRESSION_HINT
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE
   GL_TEXTURE_COMPRESSED
   GL_NUM_COMPRESSED_TEXTURE_FORMATS
   GL_COMPRESSED_TEXTURE_FORMATS

   glCompressedTexImage3D
   glCompressedTexImage2D
   glCompressedTexImage1D
   glCompressedTexSubImage3D
   glCompressedTexSubImage2D
   glCompressedTexSubImage1D
   glGetCompressedTexImage

;; F.2 ARB_texture_cube_map
   GL_NORMAL_MAP
   GL_REFLECTION_MAP
   GL_TEXTURE_CUBE_MAP
   GL_TEXTURE_BINDING_CUBE_MAP
   GL_TEXTURE_CUBE_MAP_POSITIVE_X
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
   GL_PROXY_TEXTURE_CUBE_MAP
   GL_MAX_CUBE_MAP_TEXTURE_SIZE

;; F.3 ARB_multisample
   glSampleCoverage

   GLX_SAMPLE_BUFFERS
   GLX_SAMPLES
   WGL_SAMPLE_BUFFERS
   WGL_SAMPLES
   GL_MULTISAMPLE
   GL_SAMPLE_ALPHA_TO_COVERAGE
   GL_SAMPLE_ALPHA_TO_ONE
   GL_SAMPLE_COVERAGE
   GL_MULTISAMPLE_BIT
   GL_SAMPLE_BUFFERS
   GL_SAMPLES
   GL_SAMPLE_COVERAGE_VALUE
   GL_SAMPLE_COVERAGE_INVERT

;; F.4 ARB_multitexture
	glActiveTexture
	glClientActiveTexture
	glMultiTexCoord1d
	glMultiTexCoord1dv
	glMultiTexCoord1f
	glMultiTexCoord1fv
	glMultiTexCoord1i
	glMultiTexCoord1iv
	glMultiTexCoord1s
	glMultiTexCoord1sv
	glMultiTexCoord2d
	glMultiTexCoord2dv
	glMultiTexCoord2f
	glMultiTexCoord2fv
	glMultiTexCoord2i
	glMultiTexCoord2iv
	glMultiTexCoord2s
	glMultiTexCoord2sv
	glMultiTexCoord3d
	glMultiTexCoord3dv
	glMultiTexCoord3f
	glMultiTexCoord3fv
	glMultiTexCoord3i
	glMultiTexCoord3iv
	glMultiTexCoord3s
	glMultiTexCoord3sv
	glMultiTexCoord4d
	glMultiTexCoord4dv
	glMultiTexCoord4f
	glMultiTexCoord4fv
	glMultiTexCoord4i
	glMultiTexCoord4iv
	glMultiTexCoord4s
	glMultiTexCoord4sv

	GL_ACTIVE_TEXTURE
	GL_CLIENT_ACTIVE_TEXTURE
	GL_MAX_TEXTURE_UNITS
	GL_TEXTURE0
	GL_TEXTURE1
	GL_TEXTURE2
	GL_TEXTURE3
	GL_TEXTURE4
	GL_TEXTURE5
	GL_TEXTURE6
	GL_TEXTURE7
	GL_TEXTURE8
	GL_TEXTURE9
	GL_TEXTURE10
	GL_TEXTURE11
	GL_TEXTURE12
	GL_TEXTURE13
	GL_TEXTURE14
	GL_TEXTURE15
	GL_TEXTURE16
	GL_TEXTURE17
	GL_TEXTURE18
	GL_TEXTURE19
	GL_TEXTURE20
	GL_TEXTURE21
	GL_TEXTURE22
	GL_TEXTURE23
	GL_TEXTURE24
	GL_TEXTURE25
	GL_TEXTURE26
	GL_TEXTURE27
	GL_TEXTURE28
	GL_TEXTURE29
	GL_TEXTURE30
	GL_TEXTURE31

;; F.5 ARB_texture_env_add
   ; None

;; F.6 ARB_texture_env_combine
   GL_COMBINE_RGB
   GL_COMBINE_ALPHA
   GL_SOURCE0_RGB
   GL_SOURCE1_RGB
   GL_SOURCE2_RGB
   GL_SOURCE0_ALPHA
   GL_SOURCE1_ALPHA
   GL_SOURCE2_ALPHA
   GL_OPERAND0_RGB
   GL_OPERAND1_RGB
   GL_OPERAND2_RGB
   GL_OPERAND0_ALPHA
   GL_OPERAND1_ALPHA
   GL_OPERAND2_ALPHA
   GL_RGB_SCALE
   GL_ADD_SIGNED
   GL_INTERPOLATE
   GL_SUBTRACT
   GL_CONSTANT
   GL_PRIMARY_COLOR
   GL_PREVIOUS

;; F.7 ARB_texture_env_dot3
   GL_DOT3_RGB_ARB
   GL_DOT3_RGBA_ARB

;; F.8 ARB_texture_border_clamp
   GL_CLAMP_TO_BORDER_ARB

;; F.9 ARB_transpose_matrix
   glLoadTransposeMatrixf
   glLoadTransposeMatrixd
   glMultTransposeMatrixf
   glMultTransposeMatrixd

   GL_TRANSPOSE_MODELVIEW_MATRIX
   GL_TRANSPOSE_PROJECTION_MATRIX
   GL_TRANSPOSE_TEXTURE_MATRIX
   GL_TRANSPOSE_COLOR_MATRIX
)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL 1.2))

(begin
   (define GL_VERSION_1_3 1)

   (setq GL GL_LIBRARY)

 ; F.1 ARB_texture_compression
   (define glCompressedTexImage3D (GL GLvoid "glCompressedTexImage3D" GLenum GLint GLenum GLsizei GLsizei GLsizei GLint GLsizei fft-any))
   (define glCompressedTexImage2D (GL GLvoid "glCompressedTexImage2D" GLenum GLint GLenum GLsizei GLsizei GLint GLsizei fft-any))
   (define glCompressedTexImage1D (GL GLvoid "glCompressedTexImage1D" GLenum GLint GLenum GLsizei GLint GLsizei fft-any))
   (define glCompressedTexSubImage3D (GL GLvoid "glCompressedTexSubImage3D" GLenum GLint GLint GLint GLint GLsizei GLsizei GLsizei GLenum GLsizei fft-any))
   (define glCompressedTexSubImage2D (GL GLvoid "glCompressedTexSubImage2D" GLenum GLint GLint GLint GLsizei GLsizei GLenum GLsizei fft-any))
   (define glCompressedTexSubImage1D (GL GLvoid "glCompressedTexSubImage1D" GLenum GLint GLint GLsizei GLenum GLsizei fft-any))
   (define glGetCompressedTexImage (GL GLvoid "glGetCompressedTexImage" GLenum GLint fft-any))

   (define GL_COMPRESSED_ALPHA                #x84E9)
   (define GL_COMPRESSED_LUMINANCE            #x84EA)
   (define GL_COMPRESSED_LUMINANCE_ALPHA      #x84EB)
   (define GL_COMPRESSED_INTENSITY            #x84EC)
   (define GL_COMPRESSED_RGB                  #x84ED)
   (define GL_COMPRESSED_RGBA                 #x84EE)
   (define GL_TEXTURE_COMPRESSION_HINT        #x84EF)
   (define GL_TEXTURE_COMPRESSED_IMAGE_SIZE   #x86A0)
   (define GL_TEXTURE_COMPRESSED              #x86A1)
   (define GL_NUM_COMPRESSED_TEXTURE_FORMATS  #x86A2)
   (define GL_COMPRESSED_TEXTURE_FORMATS      #x86A3)

 ; F.2 ARB_texture_cube_map
   (define GL_NORMAL_MAP                      #x8511)
   (define GL_REFLECTION_MAP                  #x8512)
   (define GL_TEXTURE_CUBE_MAP                #x8513)
   (define GL_TEXTURE_BINDING_CUBE_MAP        #x8514)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_X     #x8515)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_X     #x8516)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Y     #x8517)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y     #x8518)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Z     #x8519)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z     #x851A)
   (define GL_PROXY_TEXTURE_CUBE_MAP          #x851B)
   (define GL_MAX_CUBE_MAP_TEXTURE_SIZE       #x851C)

 ; F.3 ARB_multisample
   (define glSampleCoverage (GL GLvoid "SampleCoverage" GLclampf GLboolean))

   (define GLX_SAMPLE_BUFFERS                 100000)
   (define GLX_SAMPLES                        100001)
   (define WGL_SAMPLE_BUFFERS                 #x2041)
   (define WGL_SAMPLES                        #x2042)
   (define GL_MULTISAMPLE                     #x809D)
   (define GL_SAMPLE_ALPHA_TO_COVERAGE        #x809E)
   (define GL_SAMPLE_ALPHA_TO_ONE             #x809F)
   (define GL_SAMPLE_COVERAGE                 #x80A0)
   (define GL_MULTISAMPLE_BIT                 #x20000000)
   (define GL_SAMPLE_BUFFERS                  #x80A8)
   (define GL_SAMPLES                         #x80A9)
   (define GL_SAMPLE_COVERAGE_VALUE           #x80AA)
   (define GL_SAMPLE_COVERAGE_INVERT          #x80AB)

 ; F.4 ARB_multitexture
	(define glActiveTexture (GL GLvoid "glActiveTexture" GLenum))
	(define glClientActiveTexture (GL GLvoid "glClientActiveTexture" GLenum))
	(define glMultiTexCoord1d (GL GLvoid "glMultiTexCoord1d" GLenum GLdouble))
	(define glMultiTexCoord1dv (GL GLvoid "glMultiTexCoord1dv" GLenum GLdouble*))
	(define glMultiTexCoord1f (GL GLvoid "glMultiTexCoord1f" GLenum GLfloat))
	(define glMultiTexCoord1fv (GL GLvoid "glMultiTexCoord1fv" GLenum GLfloat*))
	(define glMultiTexCoord1i (GL GLvoid "glMultiTexCoord1i" GLenum GLint))
	(define glMultiTexCoord1iv (GL GLvoid "glMultiTexCoord1iv" GLenum GLint*))
	(define glMultiTexCoord1s (GL GLvoid "glMultiTexCoord1s" GLenum GLshort))
	(define glMultiTexCoord1sv (GL GLvoid "glMultiTexCoord1sv" GLenum GLshort*))
	(define glMultiTexCoord2d (GL GLvoid "glMultiTexCoord2d" GLenum GLdouble GLdouble))
	(define glMultiTexCoord2dv (GL GLvoid "glMultiTexCoord2dv" GLenum GLdouble*))
	(define glMultiTexCoord2f (GL GLvoid "glMultiTexCoord2f" GLenum GLfloat GLfloat))
	(define glMultiTexCoord2fv (GL GLvoid "glMultiTexCoord2fv" GLenum GLfloat*))
	(define glMultiTexCoord2i (GL GLvoid "glMultiTexCoord2i" GLenum GLint GLint))
	(define glMultiTexCoord2iv (GL GLvoid "glMultiTexCoord2iv" GLenum GLint*))
	(define glMultiTexCoord2s (GL GLvoid "glMultiTexCoord2s" GLenum GLshort GLshort))
	(define glMultiTexCoord2sv (GL GLvoid "glMultiTexCoord2sv" GLenum GLshort*))
	(define glMultiTexCoord3d (GL GLvoid "glMultiTexCoord3d" GLenum GLdouble GLdouble GLdouble))
	(define glMultiTexCoord3dv (GL GLvoid "glMultiTexCoord3dv" GLenum GLdouble*))
	(define glMultiTexCoord3f (GL GLvoid "glMultiTexCoord3f" GLenum GLfloat GLfloat GLfloat))
	(define glMultiTexCoord3fv (GL GLvoid "glMultiTexCoord3fv" GLenum GLfloat*))
	(define glMultiTexCoord3i (GL GLvoid "glMultiTexCoord3i" GLenum GLint GLint GLint))
	(define glMultiTexCoord3iv (GL GLvoid "glMultiTexCoord3iv" GLenum GLint*))
	(define glMultiTexCoord3s (GL GLvoid "glMultiTexCoord3s" GLenum GLshort GLshort GLshort))
	(define glMultiTexCoord3sv (GL GLvoid "glMultiTexCoord3sv" GLenum GLshort*))
	(define glMultiTexCoord4d (GL GLvoid "glMultiTexCoord4d" GLenum GLdouble GLdouble GLdouble GLdouble))
	(define glMultiTexCoord4dv (GL GLvoid "glMultiTexCoord4dv" GLenum GLdouble*))
	(define glMultiTexCoord4f (GL GLvoid "glMultiTexCoord4f" GLenum GLfloat GLfloat GLfloat GLfloat))
	(define glMultiTexCoord4fv (GL GLvoid "glMultiTexCoord4fv" GLenum GLfloat*))
	(define glMultiTexCoord4i (GL GLvoid "glMultiTexCoord4i" GLenum GLint GLint GLint GLint))
	(define glMultiTexCoord4iv (GL GLvoid "glMultiTexCoord4iv" GLenum GLint*))
	(define glMultiTexCoord4s (GL GLvoid "glMultiTexCoord4s" GLenum GLshort GLshort GLshort GLshort))
	(define glMultiTexCoord4sv (GL GLvoid "glMultiTexCoord4sv" GLenum GLshort*))

	(define GL_ACTIVE_TEXTURE                  #x84E0)
	(define GL_CLIENT_ACTIVE_TEXTURE           #x84E1)
	(define GL_MAX_TEXTURE_UNITS               #x84E2)
	(define GL_TEXTURE0                        #x84C0)
	(define GL_TEXTURE1                        #x84C1)
	(define GL_TEXTURE2                        #x84C2)
	(define GL_TEXTURE3                        #x84C3)
	(define GL_TEXTURE4                        #x84C4)
	(define GL_TEXTURE5                        #x84C5)
	(define GL_TEXTURE6                        #x84C6)
	(define GL_TEXTURE7                        #x84C7)
	(define GL_TEXTURE8                        #x84C8)
	(define GL_TEXTURE9                        #x84C9)
	(define GL_TEXTURE10                       #x84CA)
	(define GL_TEXTURE11                       #x84CB)
	(define GL_TEXTURE12                       #x84CC)
	(define GL_TEXTURE13                       #x84CD)
	(define GL_TEXTURE14                       #x84CE)
	(define GL_TEXTURE15                       #x84CF)
	(define GL_TEXTURE16                       #x84D0)
	(define GL_TEXTURE17                       #x84D1)
	(define GL_TEXTURE18                       #x84D2)
	(define GL_TEXTURE19                       #x84D3)
	(define GL_TEXTURE20                       #x84D4)
	(define GL_TEXTURE21                       #x84D5)
	(define GL_TEXTURE22                       #x84D6)
	(define GL_TEXTURE23                       #x84D7)
	(define GL_TEXTURE24                       #x84D8)
	(define GL_TEXTURE25                       #x84D9)
	(define GL_TEXTURE26                       #x84DA)
	(define GL_TEXTURE27                       #x84DB)
	(define GL_TEXTURE28                       #x84DC)
	(define GL_TEXTURE29                       #x84DD)
	(define GL_TEXTURE30                       #x84DE)
	(define GL_TEXTURE31                       #x84DF)

 ; F.5 ARB_texture_env_add
 ; F.6 ARB_texture_env_combine
   (define GL_COMBINE_RGB                     #x8571)
   (define GL_COMBINE_ALPHA                   #x8572)
   (define GL_SOURCE0_RGB                     #x8580)
   (define GL_SOURCE1_RGB                     #x8581)
   (define GL_SOURCE2_RGB                     #x8582)
   (define GL_SOURCE0_ALPHA                   #x8588)
   (define GL_SOURCE1_ALPHA                   #x8589)
   (define GL_SOURCE2_ALPHA                   #x858A)
   (define GL_OPERAND0_RGB                    #x8590)
   (define GL_OPERAND1_RGB                    #x8591)
   (define GL_OPERAND2_RGB                    #x8592)
   (define GL_OPERAND0_ALPHA                  #x8598)
   (define GL_OPERAND1_ALPHA                  #x8599)
   (define GL_OPERAND2_ALPHA                  #x859A)
   (define GL_RGB_SCALE                       #x8573)
   (define GL_ADD_SIGNED                      #x8574)
   (define GL_INTERPOLATE                     #x8575)
   (define GL_SUBTRACT                        #x84E7)
   (define GL_CONSTANT                        #x8576)
   (define GL_PRIMARY_COLOR                   #x8577)
   (define GL_PREVIOUS                        #x8578)

 ; F.7 ARB_texture_env_dot3
   (define GL_DOT3_RGB_ARB                    #x86AE)
   (define GL_DOT3_RGBA_ARB                   #x86AF)

 ; F.8 ARB_texture_border_clamp
   (define GL_CLAMP_TO_BORDER_ARB             #x812D)

 ; F.9 ARB_transpose_matrix
   (define glLoadTransposeMatrixf (GL GLvoid "glLoadTransposeMatrixf" (fft* fft-float)))
   (define glLoadTransposeMatrixd (GL GLvoid "glLoadTransposeMatrixd" (fft* fft-double)))
   (define glMultTransposeMatrixf (GL GLvoid "glMultTransposeMatrixf" (fft* fft-float)))
   (define glMultTransposeMatrixd (GL GLvoid "glMultTransposeMatrixd" (fft* fft-double)))

   (define GL_TRANSPOSE_MODELVIEW_MATRIX      #x84E3)
   (define GL_TRANSPOSE_PROJECTION_MATRIX     #x84E4)
   (define GL_TRANSPOSE_TEXTURE_MATRIX        #x84E5)
   (define GL_TRANSPOSE_COLOR_MATRIX          #x84E6)

))
