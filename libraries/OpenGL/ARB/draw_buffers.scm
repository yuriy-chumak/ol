; ===========================================================================
; ARB_draw_buffers                                   (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_draw_buffers.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB draw_buffers)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_draw_buffers
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_draw_buffers (gl:QueryExtension "GL_ARB_draw_buffers"))

))
