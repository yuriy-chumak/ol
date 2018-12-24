; ===========================================================================
; EXT_shadow_func                                    (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_shadow_func.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT shadow_func)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_shadow_func

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_shadow_func (gl:QueryExtension "GL_EXT_shadow_func"))

))
