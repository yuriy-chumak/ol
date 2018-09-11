; ===========================================================================
; EXT_stencil_wrap                                   (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_stencil_wrap.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT stencil_wrap)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_stencil_wrap

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_stencil_wrap (gl:QueryExtension "GL_EXT_stencil_wrap"))

))
