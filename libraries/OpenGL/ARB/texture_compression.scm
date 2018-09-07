; ===========================================================================
; ARB_texture_compression                            (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_compression.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_compression)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_compression
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_compression (gl:QueryExtension "GL_ARB_texture_compression"))

))
