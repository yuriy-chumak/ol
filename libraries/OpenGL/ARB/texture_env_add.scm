; ===========================================================================
; ARB_texture_env_add                                (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_env_add.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_env_add)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_env_add
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_add (gl:QueryExtension "GL_ARB_texture_env_add"))

))
