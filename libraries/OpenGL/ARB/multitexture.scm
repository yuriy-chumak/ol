; ===========================================================================
; ARB_multitexture                                   (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_multitexture.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB multitexture)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_multitexture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_multitexture (gl:QueryExtension "GL_ARB_multitexture"))

))
