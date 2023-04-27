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

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1)
   (OpenGL ARB multitexture)
   (OpenGL ARB texture_env_combine)
)

; ---------------------------------------------------------------------------
(export ARB_texture_env_crossbar

; ---------------------------------------------------------------------------
; New Procedures and Functions

	GL_TEXTURE0
   ;; TEXTURE<n>_ARB

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_crossbar (gl:QueryExtension "GL_ARB_texture_env_crossbar"))

	(define GL_TEXTURE0                        #x84C0)
))
