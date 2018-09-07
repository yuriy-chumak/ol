; ===========================================================================
; ARB_texture_env_combine                            (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_env_combine.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_env_combine)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_env_combine
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_combine (gl:QueryExtension "GL_ARB_texture_env_combine"))

))
