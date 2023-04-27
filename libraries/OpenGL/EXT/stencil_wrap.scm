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

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  None

; ---------------------------------------------------------------------------
(export EXT_stencil_wrap

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_INCR_WRAP_EXT
   GL_DECR_WRAP_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define EXT_stencil_wrap (gl:QueryExtension "GL_EXT_stencil_wrap"))

   (define GL_INCR_WRAP_EXT             #x8507)
   (define GL_DECR_WRAP_EXT             #x8508)

))
