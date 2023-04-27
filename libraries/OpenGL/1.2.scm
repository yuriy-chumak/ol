; OpenGL 1.2 (16 Mar 1998)
;  + EXT texture3D
;  + EXT bgra
;  + EXT packed_pixels
;  + EXT rescale_normal
;  + EXT separate_specular_color
;  + SGIS texture_edge_clamp
;  + SGIS texture_lod
;  + EXT draw_range_elements
; OpenGL 1.2.1 (14 Oct 1998)

(define-library (OpenGL 1.2)
(export
      (exports (OpenGL 1.1))

   GL_VERSION_1_2
   GL_VERSION_1_2_1

   GL_TEXTURE_BINDING_3D

   GL_UNSIGNED_BYTE_2_3_3_REV
   GL_UNSIGNED_SHORT_5_6_5
   GL_UNSIGNED_SHORT_5_6_5_REV
   GL_UNSIGNED_SHORT_4_4_4_4_REV
   GL_UNSIGNED_SHORT_1_5_5_5_REV
   GL_UNSIGNED_INT_8_8_8_8_REV
   GL_UNSIGNED_INT_2_10_10_10_REV
   GL_SMOOTH_POINT_SIZE_RANGE
   GL_SMOOTH_POINT_SIZE_GRANULARITY
   GL_SMOOTH_LINE_WIDTH_RANGE
   GL_SMOOTH_LINE_WIDTH_GRANULARITY
   GL_ALIASED_LINE_WIDTH_RANGE
   GL_ALIASED_POINT_SIZE_RANGE ; GL_VERSION_1_2_DEPRECATED

   glBlendColor
   glBlendEquation

   glTexSubImage3D ; introduced back by 1.2 (removed by 1.1)
   glCopyTexSubImage3D ; introduced back by 1.2 (removed by 1.1)

;; D.1 EXT_texture3D
   GL_PACK_SKIP_IMAGES
   GL_PACK_IMAGE_HEIGHT
   GL_UNPACK_SKIP_IMAGES
   GL_UNPACK_IMAGE_HEIGHT
   GL_TEXTURE_3D
   GL_PROXY_TEXTURE_3D
   GL_TEXTURE_DEPTH
   GL_TEXTURE_WRAP_R
   GL_MAX_3D_TEXTURE_SIZE

   glTexImage3D

;; D.2 EXT_bgra
   GL_BGR
   GL_BGRA

;; D.3 EXT_packed_pixels
   GL_UNSIGNED_BYTE_3_3_2
   GL_UNSIGNED_SHORT_4_4_4_4
   GL_UNSIGNED_SHORT_5_5_5_1
   GL_UNSIGNED_INT_8_8_8_8
   GL_UNSIGNED_INT_10_10_10_2

;; D.4 EXT_rescale_normal
   GL_RESCALE_NORMAL ; GL_VERSION_1_2_DEPRECATED

;; D.5 EXT_separate_specular_color
   GL_LIGHT_MODEL_COLOR_CONTROL ; GL_VERSION_1_2_DEPRECATED
   GL_SINGLE_COLOR              ; GL_VERSION_1_2_DEPRECATED
   GL_SEPARATE_SPECULAR_COLOR   ; GL_VERSION_1_2_DEPRECATED

;; D.6 SGIS_texture_edge_clamp
   GL_CLAMP_TO_EDGE

;; D.7 SGIS_texture_lod
   GL_TEXTURE_MIN_LOD
   GL_TEXTURE_MAX_LOD
   GL_TEXTURE_BASE_LEVEL
   GL_TEXTURE_MAX_LEVEL

;; D.8 EXT_draw_range_elements
   GL_MAX_ELEMENTS_VERTICES
   GL_MAX_ELEMENTS_INDICES

   glDrawRangeElements
)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL 1.1))

(begin
   (define GL_VERSION_1_2 1)
   (define GL_VERSION_1_2_1 1)

   (setq GL GL_LIBRARY)

   (define GL_TEXTURE_BINDING_3D             #x806A)

   (define GL_UNSIGNED_BYTE_2_3_3_REV        #x8362)
   (define GL_UNSIGNED_SHORT_5_6_5           #x8363)
   (define GL_UNSIGNED_SHORT_5_6_5_REV       #x8364)
   (define GL_UNSIGNED_SHORT_4_4_4_4_REV     #x8365)
   (define GL_UNSIGNED_SHORT_1_5_5_5_REV     #x8366)
   (define GL_UNSIGNED_INT_8_8_8_8_REV       #x8367)
   (define GL_UNSIGNED_INT_2_10_10_10_REV    #x8368)
   (define GL_SMOOTH_POINT_SIZE_RANGE        #x0B12)
   (define GL_SMOOTH_POINT_SIZE_GRANULARITY  #x0B13)
   (define GL_SMOOTH_LINE_WIDTH_RANGE        #x0B22)
   (define GL_SMOOTH_LINE_WIDTH_GRANULARITY  #x0B23)
   (define GL_ALIASED_LINE_WIDTH_RANGE       #x846E)
   (define GL_ALIASED_POINT_SIZE_RANGE       #x846D)

   (define glBlendColor (GL GLvoid "glBlendColor" GLclampf GLclampf GLclampf GLclampf))
   (define glBlendEquation (GL GLvoid "glBlendEquation" GLenum))

   ; introduced back by 1.2 (excluded in 1.1)
   (define glTexSubImage3D (GL GLvoid "glTexSubImage3D" GLenum GLint GLint GLint GLint GLsizei GLsizei GLsizei GLenum GLenum fft-any))
   (define glCopyTexSubImage3D (GL GLvoid "glCopyTexSubImage3D" GLenum GLint GLint GLint GLint GLint GLint GLsizei GLsizei))

 ; D.1 EXT_texture3D
   (define GL_PACK_SKIP_IMAGES        #x806B)
   (define GL_PACK_IMAGE_HEIGHT       #x806C)
   (define GL_UNPACK_SKIP_IMAGES      #x806D)
   (define GL_UNPACK_IMAGE_HEIGHT     #x806E)
   (define GL_TEXTURE_3D              #x806F)
   (define GL_PROXY_TEXTURE_3D        #x8070)
   (define GL_TEXTURE_DEPTH           #x8071)
   (define GL_TEXTURE_WRAP_R          #x8072)
   (define GL_MAX_3D_TEXTURE_SIZE     #x8073)

   (define glTexImage3D (GL GLvoid "glTexImage3D" GLenum GLint GLint GLsizei GLsizei GLsizei GLint GLenum GLenum fft-any))

 ; D.2 EXT_bgra
   (define GL_BGR                     #x80E0)
   (define GL_BGRA                    #x80E1)

 ; D.3 EXT_packed_pixels
   (define GL_UNSIGNED_BYTE_3_3_2     #x8032)
   (define GL_UNSIGNED_SHORT_4_4_4_4  #x8033)
   (define GL_UNSIGNED_SHORT_5_5_5_1  #x8034)
   (define GL_UNSIGNED_INT_8_8_8_8    #x8035)
   (define GL_UNSIGNED_INT_10_10_10_2 #x8036)

 ; D.4 EXT_rescale_normal
   (define GL_RESCALE_NORMAL          #x803A)

 ; D.5 EXT_separate_specular_color
   (define GL_LIGHT_MODEL_COLOR_CONTROL      #x81F8)
   (define GL_SINGLE_COLOR                   #x81F9)
   (define GL_SEPARATE_SPECULAR_COLOR        #x81FA)

 ; D.6 SGIS_texture_edge_clamp
   (define GL_CLAMP_TO_EDGE		     #x812F)

 ; D.7 SGIS_texture_lod
   (define GL_TEXTURE_MIN_LOD         #x813A)
   (define GL_TEXTURE_MAX_LOD         #x813B)
   (define GL_TEXTURE_BASE_LEVEL      #x813C)
   (define GL_TEXTURE_MAX_LEVEL       #x813D)

 ; D.8 EXT_draw_range_elements
   (define GL_MAX_ELEMENTS_VERTICES  #x80E8)
   (define GL_MAX_ELEMENTS_INDICES   #x80E9)

   (define glDrawRangeElements (GL GLvoid "glDrawRangeElements" GLenum GLuint GLuint GLsizei GLenum fft-any))
))
