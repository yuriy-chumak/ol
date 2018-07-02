; OpenGL 1.2 (1998), OpenGL 1.2.1 (1998)
; в комментриях указаны расширения, которые были интегрированы в этот выпуск
; ==========================================================================
(define-library (OpenGL version-1-2)
(export

   GL_VERSION_1_2
   GL_VERSION_1_2_1
  
 ; removed by 1.1 (introduced back by 1.2)
   ;glTexSubImage3D ; void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels)
   ;glCopyTexSubImage3D ; void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height)

 ; 1.2
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
   GL_ALIASED_POINT_SIZE_RANGE


 ; EXT_texture3D
   GL_PACK_SKIP_IMAGES
   GL_PACK_IMAGE_HEIGHT
   GL_UNPACK_SKIP_IMAGES
   GL_UNPACK_IMAGE_HEIGHT
   GL_TEXTURE_3D
   GL_PROXY_TEXTURE_3D
   GL_TEXTURE_DEPTH
   GL_TEXTURE_WRAP_R
   GL_MAX_3D_TEXTURE_SIZE
   ;glTexImage3D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void *pixels)

 ; EXT_packed_pixels +
   GL_UNSIGNED_BYTE_3_3_2
   GL_UNSIGNED_SHORT_4_4_4_4
   GL_UNSIGNED_SHORT_5_5_5_1
   GL_UNSIGNED_INT_8_8_8_8
   GL_UNSIGNED_INT_10_10_10_2

 ; SGIS_texture_lod
   GL_TEXTURE_MIN_LOD
   GL_TEXTURE_MAX_LOD
   GL_TEXTURE_BASE_LEVEL
   GL_TEXTURE_MAX_LEVEL

 ; EXT_rescale_normal
   GL_RESCALE_NORMAL

 ; SGIS_texture_edge_clamp
   GL_CLAMP_TO_EDGE

 ; EXT_draw_range_elements
   ;glDrawRangeElements ; void (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices);

   GL_MAX_ELEMENTS_VERTICES
   GL_MAX_ELEMENTS_INDICES

 ; EXT_bgra
   GL_BGR
   GL_BGRA

 ; EXT_separate_specular_color
   GL_LIGHT_MODEL_COLOR_CONTROL
   GL_SINGLE_COLOR
   GL_SEPARATE_SPECULAR_COLOR

   (exports (OpenGL version-1-1)))

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
   (OpenGL version-1-1))

(begin
   (define GL_VERSION_1_2 1)
   (define GL_VERSION_1_2_1 1)

   (define GL GL_LIBRARY)

   ;GLAPI void APIENTRY glTexSubImage3D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type, const void *pixels);
   ;GLAPI void APIENTRY glCopyTexSubImage3D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset, GLint x, GLint y, GLsizei width, GLsizei height);

 ; 1.2
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


 ; EXT_texture3D
   (define GL_PACK_SKIP_IMAGES     #x806B)
   (define GL_PACK_IMAGE_HEIGHT    #x806C)
   (define GL_UNPACK_SKIP_IMAGES   #x806D)
   (define GL_UNPACK_IMAGE_HEIGHT  #x806E)
   (define GL_TEXTURE_3D           #x806F)
   (define GL_PROXY_TEXTURE_3D     #x8070)
   (define GL_TEXTURE_DEPTH        #x8071)
   (define GL_TEXTURE_WRAP_R       #x8072)
   (define GL_MAX_3D_TEXTURE_SIZE  #x8073)

   ;GLAPI void APIENTRY glTexImage3D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLsizei depth, GLint border, GLenum format, GLenum type, const void *pixels);

 ; EXT_packed_pixels
   (define GL_UNSIGNED_BYTE_3_3_2     #x8032)
   (define GL_UNSIGNED_SHORT_4_4_4_4  #x8033)
   (define GL_UNSIGNED_SHORT_5_5_5_1  #x8034)
   (define GL_UNSIGNED_INT_8_8_8_8    #x8035)
   (define GL_UNSIGNED_INT_10_10_10_2 #x8036)

 ; SGIS_texture_lod
   (define GL_TEXTURE_MIN_LOD         #x813A)
   (define GL_TEXTURE_MAX_LOD         #x813B)
   (define GL_TEXTURE_BASE_LEVEL      #x813C)
   (define GL_TEXTURE_MAX_LEVEL       #x813D)

 ; EXT_rescale_normal
   (define GL_RESCALE_NORMAL          #x803A)

 ; SGIS_texture_edge_clamp
   (define GL_CLAMP_TO_EDGE		     #x812F)

 ; EXT_draw_range_elements
   (define GL_MAX_ELEMENTS_VERTICES  #x80E8)
   (define GL_MAX_ELEMENTS_INDICES   #x80E9)

   ;GLAPI void APIENTRY glDrawRangeElements (GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices);

 ; EXT_bgra
   (define GL_BGR                    #x80E0)
   (define GL_BGRA                   #x80E1)

 ; EXT_separate_specular_color
   (define GL_LIGHT_MODEL_COLOR_CONTROL #x81F8)
   (define GL_SINGLE_COLOR              #x81F9)
   (define GL_SEPARATE_SPECULAR_COLOR   #x81FA)
))
