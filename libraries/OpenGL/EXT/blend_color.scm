; ===========================================================================
; EXT_blend_color                                    (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_color.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_color)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_blend_color

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_color (gl:QueryExtension "GL_EXT_blend_color"))

))
