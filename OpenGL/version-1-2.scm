; OpenGL 1.2 (1998), OpenGL 1.2.1 (1998)
; в комментриях указаны расширения, которые были интегрированы в этот выпуск
; ==========================================================================
(define-library (OpenGL version-1-2)
(export

   GL_VERSION_1_2
   GL_VERSION_1_2_1
  

 ; EXT_texture3D
   ;TexImage3DEXT
;
;        PACK_SKIP_IMAGES_EXT             0x806B
;        PACK_IMAGE_HEIGHT_EXT            0x806C
;        UNPACK_SKIP_IMAGES_EXT           0x806D
;        UNPACK_IMAGE_HEIGHT_EXT          0x806E
;
;        TEXTURE_3D_EXT                   0x806F
;
;        PROXY_TEXTURE_3D_EXT             0x8070
;
;        TEXTURE_DEPTH_EXT                0x8071
;
;        TEXTURE_WRAP_R_EXT               0x8072
;
;        MAX_3D_TEXTURE_SIZE_EXT          0x8073

 ; EXT_packed_pixels
;        UNSIGNED_BYTE_3_3_2_EXT         0x8032
;        UNSIGNED_SHORT_4_4_4_4_EXT      0x8033
;        UNSIGNED_SHORT_5_5_5_1_EXT      0x8034
;        UNSIGNED_INT_8_8_8_8_EXT        0x8035
;        UNSIGNED_INT_10_10_10_2_EXT     0x8036

 ; SGIS_texture_lod
;        TEXTURE_MIN_LOD_SGIS            0x813A
;        TEXTURE_MAX_LOD_SGIS            0x813B
;        TEXTURE_BASE_LEVEL_SGIS         0x813C
;        TEXTURE_MAX_LEVEL_SGIS          0x813D

 ; EXT_rescale_normal
;    RESCALE_NORMAL_EXT 0x803A

 ; SGIS_texture_edge_clamp
;   CLAMP_TO_EDGE_SGIS		0x812F

 ; EXT_draw_range_elements
;glDrawRangeElementsEXT
;
;GL_MAX_ELEMENTS_VERTICES_EXT  0x80E8
;GL_MAX_ELEMENTS_INDICES_EXT   0x80E9

 ; EXT_bgra
;        BGR_EXT                 0x80E0
;        BGRA_EXT                0x80E1

 ; EXT_separate_specular_color
;    LIGHT_MODEL_COLOR_CONTROL_EXT	0x81F8
;
;    SINGLE_COLOR_EXT			0x81F9
;    SEPARATE_SPECULAR_COLOR_EXT		0x81FA

   (exports (OpenGL version-1-1)))

; ============================================================================
; == implementation ==========================================================
(import (r5rs core)
   (OpenGL version-1-1))

(begin
   (define GL_VERSION_1_2 1)
   (define GL_VERSION_1_2_1 1)

   (define GL GL_LIBRARY)

;  ; opengl 1.2 https://www.opengl.org/registry/api/GL/glext.h

   (define GLU_VERSION_1_3 1)
))
