; ===========================================================================
; EXT_blend_squation_separate                        (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_squation_separate.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_squation_separate)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_blend_squation_separate

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_squation_separate (gl:QueryExtension "GL_EXT_blend_squation_separate"))

))
