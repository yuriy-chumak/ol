; ===========================================================================
; ARB_texture_env_dot3                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_env_dot3.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_env_dot3)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_env_dot3

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_dot3 (gl:QueryExtension "GL_ARB_texture_env_dot3"))

))
