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
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_packed_pixels

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_packed_pixels (gl:QueryExtension "GL_EXT_packed_pixels"))

))
