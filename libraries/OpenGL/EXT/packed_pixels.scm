; ===========================================================================
; EXT_packed_pixels                                  (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_packed_pixels.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT packed_pixels)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;    EXT_abgr affects the definition of this extension
;    EXT_texture3D affects the definition of this extension
;    EXT_subtexture affects the definition of this extension
;    EXT_histogram affects the definition of this extension
;    EXT_convolution affects the definition of this extension
;    SGI_color_table affects the definition of this extension
;    SGIS_texture4D affects the definition of this extension
;    EXT_cmyka affects the definition of this extension

; ---------------------------------------------------------------------------
(export EXT_packed_pixels

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_UNSIGNED_BYTE_3_3_2_EXT
   GL_UNSIGNED_SHORT_4_4_4_4_EXT
   GL_UNSIGNED_SHORT_5_5_5_1_EXT
   GL_UNSIGNED_INT_8_8_8_8_EXT
   GL_UNSIGNED_INT_10_10_10_2_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_packed_pixels (gl:QueryExtension "GL_EXT_packed_pixels"))

   (define GL_UNSIGNED_BYTE_3_3_2_EXT         #x8032)
   (define GL_UNSIGNED_SHORT_4_4_4_4_EXT      #x8033)
   (define GL_UNSIGNED_SHORT_5_5_5_1_EXT      #x8034)
   (define GL_UNSIGNED_INT_8_8_8_8_EXT        #x8035)
   (define GL_UNSIGNED_INT_10_10_10_2_EXT     #x8036)

))
