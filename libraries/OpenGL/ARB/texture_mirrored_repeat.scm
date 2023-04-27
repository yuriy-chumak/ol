; ===========================================================================
; ARB_texture_mirrored_repeat                        (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_mirrored_repeat.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_mirrored_repeat)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;

; ---------------------------------------------------------------------------
(export ARB_texture_mirrored_repeat

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_MIRRORED_REPEAT_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_mirrored_repeat (gl:QueryExtension "GL_ARB_texture_mirrored_repeat"))

   (define GL_MIRRORED_REPEAT_ARB                    #x8370)
))
