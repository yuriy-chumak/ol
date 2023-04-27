; ===========================================================================
; ARB_texture_border_clamp                           (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_border_clamp.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_border_clamp)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.0))

; ---------------------------------------------------------------------------
(export ARB_texture_border_clamp

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_CLAMP_TO_BORDER_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_border_clamp (gl:QueryExtension "GL_ARB_texture_border_clamp"))

   (define GL_CLAMP_TO_BORDER_ARB                             #x812D)

))
