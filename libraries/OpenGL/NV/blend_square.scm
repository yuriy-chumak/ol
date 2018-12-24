; ===========================================================================
; NV_blend_square                                    (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/NV/NV_blend_square.txt
;
; Version
;
; Overview
;
(define-library (OpenGL NV blend_square)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export NV_blend_square

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define NV_blend_square (gl:QueryExtension "GL_NV_blend_square"))

))
