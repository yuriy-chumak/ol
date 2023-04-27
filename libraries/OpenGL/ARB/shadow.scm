; ===========================================================================
; ARB_shadow                                         (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shadow.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shadow)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1)
   (OpenGL ARB depth_texture)
)

; ---------------------------------------------------------------------------
(export ARB_shadow

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_TEXTURE_COMPARE_MODE_ARB
   GL_TEXTURE_COMPARE_FUNC_ARB

   GL_COMPARE_R_TO_TEXTURE_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_shadow (gl:QueryExtension "GL_ARB_shadow"))

   (define GL_TEXTURE_COMPARE_MODE_ARB    #x884C)
   (define GL_TEXTURE_COMPARE_FUNC_ARB    #x884D)
   (define GL_COMPARE_R_TO_TEXTURE_ARB    #x884E)

))
