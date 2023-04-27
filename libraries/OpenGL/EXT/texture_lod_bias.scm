; ===========================================================================
; EXT_texture_lod_bias                               (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_lod_bias.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT texture_lod_bias)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;

; ---------------------------------------------------------------------------
(export EXT_texture_lod_bias

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_TEXTURE_FILTER_CONTROL_EXT
   GL_TEXTURE_LOD_BIAS_EXT
   GL_MAX_TEXTURE_LOD_BIAS_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture_lod_bias (gl:QueryExtension "GL_EXT_texture_lod_bias"))

   (define GL_TEXTURE_FILTER_CONTROL_EXT          #x8500)
   (define GL_TEXTURE_LOD_BIAS_EXT                #x8501)
   (define GL_MAX_TEXTURE_LOD_BIAS_EXT            #x84FD)
))
