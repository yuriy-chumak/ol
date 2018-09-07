; ===========================================================================
; ARB_shadow                                         (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shadow.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shadow)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_shadow
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_shadow (gl:QueryExtension "GL_ARB_shadow"))

))
