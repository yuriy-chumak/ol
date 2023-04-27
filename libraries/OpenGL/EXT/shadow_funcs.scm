; ===========================================================================
; EXT_shadow_func                                    (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_shadow_funcs.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT shadow_funcs)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1)
   (OpenGL ARB depth_texture)
   (OpenGL ARB shadow)
)

; ---------------------------------------------------------------------------
(export EXT_shadow_funcs

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_shadow_funcs (gl:QueryExtension "GL_EXT_shadow_funcs"))

))
