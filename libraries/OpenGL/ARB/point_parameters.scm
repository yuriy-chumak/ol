; ===========================================================================
; ARB_point_parameters                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_point_parameters.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB point_parameters)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_point_parameters
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_point_parameters (gl:QueryExtension "GL_ARB_point_parameters"))

))
