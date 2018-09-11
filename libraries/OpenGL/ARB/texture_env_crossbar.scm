; ===========================================================================
; ARB_texture_env_crossbar                           (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_env_crossbar.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_env_crossbar)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_env_crossbar

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_crossbar (gl:QueryExtension "GL_ARB_texture_env_crossbar"))

))
