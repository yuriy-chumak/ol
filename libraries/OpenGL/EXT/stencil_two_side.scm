; ===========================================================================
; EXT_stencil_two_side                               (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_stencil_two_side.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT stencil_two_side)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_stencil_two_side

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_stencil_two_side (gl:QueryExtension "GL_EXT_stencil_two_side"))

))
