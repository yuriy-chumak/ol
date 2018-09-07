; ===========================================================================
; ARB_vertex_shader                                  (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_vertex_shader.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB vertex_shader)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_vertex_shader
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_vertex_shader (gl:QueryExtension "GL_ARB_vertex_shader"))

))
