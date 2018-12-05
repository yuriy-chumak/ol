; ===========================================================================
; ATI_separate_stencil                               (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ATI/separate_stencil.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ATI separate_stencil)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ATI_separate_stencil

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ATI_separate_stencil (gl:QueryExtension "GL_ATI_separate_stencil"))

))
