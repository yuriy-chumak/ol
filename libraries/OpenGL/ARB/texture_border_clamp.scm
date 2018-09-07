; ===========================================================================
; ARB_texture_border_clamp                           (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_border_clamp.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_border_clamp)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_border_clamp
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_border_clamp (gl:QueryExtension "GL_ARB_texture_border_clamp"))

))
