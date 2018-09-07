; ===========================================================================
; ARB_multisample                                    (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_multisample.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB multisample)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_multisample
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_multisample (gl:QueryExtension "GL_ARB_multisample"))

))
