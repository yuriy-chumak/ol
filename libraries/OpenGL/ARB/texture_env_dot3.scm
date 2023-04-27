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
(export ARB_texture_env_dot3

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_DOT3_RGB_ARB
   GL_DOT3_RGBA_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_dot3 (gl:QueryExtension "GL_ARB_texture_env_dot3"))

   (define GL_DOT3_RGB_ARB                                    #x86AE)
   (define GL_DOT3_RGBA_ARB                                   #x86AF)

))
