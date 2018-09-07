; ===========================================================================
; ARB_point_sprite                                   (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_point_sprite.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB point_sprite)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_point_sprite
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_point_sprite (gl:QueryExtension "GL_ARB_point_sprite"))

))
