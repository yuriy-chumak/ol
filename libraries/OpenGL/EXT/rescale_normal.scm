; ===========================================================================
; EXT_rescale_normal                                 (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_rescale_normal.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT rescale_normal)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  None

; ---------------------------------------------------------------------------
(export EXT_rescale_normal

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_RESCALE_NORMAL_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define EXT_rescale_normal (gl:QueryExtension "GL_EXT_rescale_normal"))

   (define GL_RESCALE_NORMAL_EXT #x803A)

))
