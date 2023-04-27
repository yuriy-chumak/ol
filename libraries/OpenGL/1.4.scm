; OpenGL 1.4 (24 Jul 2002)
;  + SGIS generate_mipmap
;  + NV blend_square
;  + The subset of blending features
;  + ARB depth_texture, ARB shadow
;  + EXT fog_coord
;  + EXT multi_draw_arrays
;  + ARB point_parameters
;  + EXT secondary_color
;  + EXT blend_func_separate
;  + EXT stencil_wrap
;  + ARB texture_env_crossbar
;  + EXT texture_lod_bias
;  + ARB texture_mirrored_repeat
;  + ARB window_pos

; ?? EXT_blend_color
; ?? EXT_blend_subtract
; ?? EXT_blend_minmax

(define-library (OpenGL 1.4)
(export
      (exports (OpenGL 1.3))

   GL_VERSION_1_4

   GL_BLEND_COLOR
   GL_BLEND_EQUATION
   GL_CONSTANT_COLOR
   GL_ONE_MINUS_CONSTANT_COLOR
   GL_CONSTANT_ALPHA
   GL_ONE_MINUS_CONSTANT_ALPHA
   GL_FUNC_ADD
   GL_FUNC_REVERSE_SUBTRACT
   GL_FUNC_SUBTRACT
   GL_MIN
   GL_MAX

;; G.1 SGIS_generate_mipmap
   GL_GENERATE_MIPMAP
   GL_GENERATE_MIPMAP_HINT

;; G.2 NV_blend_square

;; G.3 The subset of blending features

;; G.4 ARB_depth_texture, ARB_shadow
   GL_DEPTH_COMPONENT16
   GL_DEPTH_COMPONENT24
   GL_DEPTH_COMPONENT32
   GL_TEXTURE_DEPTH_SIZE
   GL_DEPTH_TEXTURE_MODE
   GL_TEXTURE_COMPARE_MODE
   GL_TEXTURE_COMPARE_FUNC
   GL_COMPARE_R_TO_TEXTURE

;; G.5 EXT_fog_coord
   glFogCoordf
   glFogCoordd
   glFogCoordfv
   glFogCoorddv
   glFogCoordPointer

   GL_FOG_COORDINATE_SOURCE
   GL_FOG_COORDINATE
   GL_FRAGMENT_DEPTH
   GL_CURRENT_FOG_COORDINATE
   GL_FOG_COORDINATE_ARRAY_TYPE
   GL_FOG_COORDINATE_ARRAY_STRIDE
   GL_FOG_COORDINATE_ARRAY_POINTER
   GL_FOG_COORDINATE_ARRAY

;; G.6 EXT_multi_draw_arrays
   glMultiDrawArrays
   glMultiDrawElements

;; G.7 ARB_point_parameters
   glPointParameterf
   glPointParameterfv
   glPointParameteri ; 1.4
   glPointParameteriv ; 1.4

   GL_POINT_SIZE_MIN
   GL_POINT_SIZE_MAX
   GL_POINT_FADE_THRESHOLD_SIZE
   GL_POINT_DISTANCE_ATTENUATION

;; G.8 EXT_secondary_color
   glSecondaryColor3b
   glSecondaryColor3bv
   glSecondaryColor3d
   glSecondaryColor3dv
   glSecondaryColor3f
   glSecondaryColor3fv
   glSecondaryColor3i
   glSecondaryColor3iv
   glSecondaryColor3s
   glSecondaryColor3sv
   glSecondaryColor3ub
   glSecondaryColor3ubv
   glSecondaryColor3ui
   glSecondaryColor3uiv
   glSecondaryColor3us
   glSecondaryColor3usv
   glSecondaryColorPointer

   GL_COLOR_SUM
   GL_CURRENT_SECONDARY_COLOR
   GL_SECONDARY_COLOR_ARRAY_SIZE
   GL_SECONDARY_COLOR_ARRAY_TYPE
   GL_SECONDARY_COLOR_ARRAY_STRIDE
   GL_SECONDARY_COLOR_ARRAY_POINTER
   GL_SECONDARY_COLOR_ARRAY

;; G.9 EXT_blend_func_separate
   glBlendFuncSeparate

   GL_BLEND_DST_RGB
   GL_BLEND_SRC_RGB
   GL_BLEND_DST_ALPHA
   GL_BLEND_SRC_ALPHA

;; G.10 EXT_stencil_wrap
   GL_INCR_WRAP
   GL_DECR_WRAP

;; G.11 ARB_texture_env_crossbar

;; G.12 EXT_texture_lod_bias
   GL_TEXTURE_FILTER_CONTROL
   GL_TEXTURE_LOD_BIAS
   GL_MAX_TEXTURE_LOD_BIAS

;; G.13 ARB_texture_mirrored_repeat
   GL_MIRRORED_REPEAT

;; G.14 ARB_window_pos
   glWindowPos2d
   glWindowPos2dv
   glWindowPos2f
   glWindowPos2fv
   glWindowPos2i
   glWindowPos2iv
   glWindowPos2s
   glWindowPos2sv
   glWindowPos3d
   glWindowPos3dv
   glWindowPos3f
   glWindowPos3fv
   glWindowPos3i
   glWindowPos3iv
   glWindowPos3s
   glWindowPos3sv

)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL 1.3))

(begin
   (define GL_VERSION_1_4 1)

   (setq GL GL_LIBRARY)

   (define GL_BLEND_COLOR              #x8005)
   (define GL_BLEND_EQUATION           #x8009)
   (define GL_CONSTANT_COLOR           #x8001)
   (define GL_ONE_MINUS_CONSTANT_COLOR #x8002)
   (define GL_CONSTANT_ALPHA           #x8003)
   (define GL_ONE_MINUS_CONSTANT_ALPHA #x8004)
   (define GL_FUNC_ADD                 #x8006)
   (define GL_FUNC_REVERSE_SUBTRACT    #x800B)
   (define GL_FUNC_SUBTRACT            #x800A)
   (define GL_MIN                      #x8007)
   (define GL_MAX                      #x8008)

 ; G.1 SGIS_generate_mipmap
   (define GL_GENERATE_MIPMAP          #x8191)
   (define GL_GENERATE_MIPMAP_HINT     #x8192)

 ; G.2 NV_blend_square

 ; G.3 The subset of blending features

 ; G.4 ARB_depth_texture, ARB_shadow
   (define GL_DEPTH_COMPONENT16        #x81A5)
   (define GL_DEPTH_COMPONENT24        #x81A6)
   (define GL_DEPTH_COMPONENT32        #x81A7)
   (define GL_TEXTURE_DEPTH_SIZE       #x884A)
   (define GL_DEPTH_TEXTURE_MODE       #x884B)

   (define GL_TEXTURE_COMPARE_MODE     #x884C)
   (define GL_TEXTURE_COMPARE_FUNC     #x884D)
   (define GL_COMPARE_R_TO_TEXTURE     #x884E)

 ; G.5 EXT_fog_coord
   (define glFogCoordf (GL GLvoid "glFogCoordf" GLfloat))
   (define glFogCoordd (GL GLvoid "glFogCoordd" GLdouble))
   (define glFogCoordfv (GL GLvoid "glFogCoordfv" GLfloat*))
   (define glFogCoorddv (GL GLvoid "glFogCoorddv" GLdouble*))
   (define glFogCoordPointer (GL GLvoid "glFogCoordPointer" GLenum GLsizei fft-any))

   (define GL_FOG_COORDINATE_SOURCE        #x8450)
   (define GL_FOG_COORDINATE               #x8451)
   (define GL_FRAGMENT_DEPTH               #x8452)
   (define GL_CURRENT_FOG_COORDINATE       #x8453)
   (define GL_FOG_COORDINATE_ARRAY_TYPE    #x8454)
   (define GL_FOG_COORDINATE_ARRAY_STRIDE  #x8455)
   (define GL_FOG_COORDINATE_ARRAY_POINTER #x8456)
   (define GL_FOG_COORDINATE_ARRAY         #x8457)

 ; G.6 EXT_multi_draw_arrays
   (define glMultiDrawArrays (GL GLvoid "glMultiDrawArrays" GLenum GLint* (fft* GLsizei) GLsizei))
   (define glMultiDrawElements (GL GLvoid "glMultiDrawElements" GLenum (fft* GLsizei) GLenum fft-any GLsizei))

 ; G.7 ARB_point_parameters
   (define glPointParameterf (GL GLvoid "glPointParameterf" GLenum GLfloat))
   (define glPointParameterfv (GL GLvoid "glPointParameterfv" GLenum GLfloat*))
   (define glPointParameteri (GL GLvoid "glPointParameteri" GLenum GLint))
   (define glPointParameteriv (GL GLvoid "glPointParameteifv" GLenum GLint*))

   (define GL_POINT_SIZE_MIN               #x8126)
   (define GL_POINT_SIZE_MAX               #x8127)
   (define GL_POINT_FADE_THRESHOLD_SIZE    #x8128)
   (define GL_POINT_DISTANCE_ATTENUATION   #x8129)

 ; G.8 EXT_secondary_color
   (define glSecondaryColor3b (GL GLvoid "glSecondaryColor3b" GLbyte GLbyte GLbyte))
   (define glSecondaryColor3bv (GL GLvoid "glSecondaryColor3bv" GLbyte*))
   (define glSecondaryColor3d (GL GLvoid "glSecondaryColor3d" GLdouble GLdouble GLdouble))
   (define glSecondaryColor3dv (GL GLvoid "glSecondaryColor3dv" GLdouble*))
   (define glSecondaryColor3f (GL GLvoid "glSecondaryColor3f" GLfloat GLfloat GLfloat))
   (define glSecondaryColor3fv (GL GLvoid "glSecondaryColor3fv" GLfloat*))
   (define glSecondaryColor3i (GL GLvoid "glSecondaryColor3i" GLint GLint GLint))
   (define glSecondaryColor3iv (GL GLvoid "glSecondaryColor3iv" GLint*))
   (define glSecondaryColor3s (GL GLvoid "glSecondaryColor3s" GLshort GLshort GLshort))
   (define glSecondaryColor3sv (GL GLvoid "glSecondaryColor3sv" GLshort*))
   (define glSecondaryColor3ub (GL GLvoid "glSecondaryColor3ub" GLubyte GLubyte GLubyte))
   (define glSecondaryColor3ubv (GL GLvoid "glSecondaryColor3ubv" GLubyte*))
   (define glSecondaryColor3ui (GL GLvoid "glSecondaryColor3ui" GLuint GLuint GLuint))
   (define glSecondaryColor3uiv (GL GLvoid "glSecondaryColor3uiv" GLuint*))
   (define glSecondaryColor3us (GL GLvoid "glSecondaryColor3us" GLushort GLushort GLushort))
   (define glSecondaryColor3usv (GL GLvoid "glSecondaryColor3usv" GLushort*))
   (define glSecondaryColorPointer (GL GLvoid "glSecondaryColorPointer" GLint GLenum GLsizei fft-any))

   (define GL_COLOR_SUM                      #x8458)
   (define GL_CURRENT_SECONDARY_COLOR        #x8459)
   (define GL_SECONDARY_COLOR_ARRAY_SIZE     #x845A)
   (define GL_SECONDARY_COLOR_ARRAY_TYPE     #x845B)
   (define GL_SECONDARY_COLOR_ARRAY_STRIDE   #x845C)
   (define GL_SECONDARY_COLOR_ARRAY_POINTER  #x845D)
   (define GL_SECONDARY_COLOR_ARRAY          #x845E)

 ; G.9 EXT_blend_func_separate
   (define glBlendFuncSeparate (GL GLvoid "BlendFuncSeparate" GLenum GLenum GLenum GLenum))

   (define GL_BLEND_DST_RGB                  #x80C8)
   (define GL_BLEND_SRC_RGB                  #x80C9)
   (define GL_BLEND_DST_ALPHA                #x80CA)
   (define GL_BLEND_SRC_ALPHA                #x80CB)

 ; G.10 EXT_stencil_wrap
   (define GL_INCR_WRAP                      #x8507)
   (define GL_DECR_WRAP                      #x8508)

 ; G.11 ARB_texture_env_crossbar

 ; G.12 EXT_texture_lod_bias
   (define GL_TEXTURE_FILTER_CONTROL         #x8500)
   (define GL_TEXTURE_LOD_BIAS               #x8501)
   (define GL_MAX_TEXTURE_LOD_BIAS           #x84FD)

 ; G.13 ARB_texture_mirrored_repeat
   (define GL_MIRRORED_REPEAT                #x8370)

 ; G.14 ARB_window_pos
   (define glWindowPos2d (GL GLvoid "glWindowPos2d" GLdouble GLdouble))
   (define glWindowPos2dv (GL GLvoid "glWindowPos2dv" GLdouble*))
   (define glWindowPos2f (GL GLvoid "glWindowPos2f" GLfloat GLfloat))
   (define glWindowPos2fv (GL GLvoid "glWindowPos2fv" GLfloat*))
   (define glWindowPos2i (GL GLvoid "glWindowPos2i" GLint GLint))
   (define glWindowPos2iv (GL GLvoid "glWindowPos2iv" GLint*))
   (define glWindowPos2s (GL GLvoid "glWindowPos2s" GLshort GLshort))
   (define glWindowPos2sv (GL GLvoid "glWindowPos2sv" GLshort*))
   (define glWindowPos3d (GL GLvoid "glWindowPos3d" GLdouble GLdouble GLdouble))
   (define glWindowPos3dv (GL GLvoid "glWindowPos3dv" GLdouble*))
   (define glWindowPos3f (GL GLvoid "glWindowPos3f" GLfloat GLfloat GLfloat))
   (define glWindowPos3fv (GL GLvoid "glWindowPos3fv" GLfloat*))
   (define glWindowPos3i (GL GLvoid "glWindowPos3i" GLint GLint GLint))
   (define glWindowPos3iv (GL GLvoid "glWindowPos3iv" GLint*))
   (define glWindowPos3s (GL GLvoid "glWindowPos3s" GLshort GLshort GLshort))
   (define glWindowPos3sv (GL GLvoid "glWindowPos3sv" GLshort*))

))
