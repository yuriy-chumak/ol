; ===========================================================================
; EXT_blend_func_separate                            (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_func_separate.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_func_separate)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_blend_func_separate

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_func_separate (gl:QueryExtension "GL_EXT_blend_func_separate"))

))
