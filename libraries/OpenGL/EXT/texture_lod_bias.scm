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

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_texture_lod_bias

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture_lod_bias (gl:QueryExtension "GL_EXT_texture_lod_bias"))

))
