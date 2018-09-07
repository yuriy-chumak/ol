; ===========================================================================
; ARB_shader_objects                                 (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shader_objects.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shader_objects)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_shader_objects
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_shader_objects (gl:QueryExtension "GL_ARB_shader_objects"))

))
