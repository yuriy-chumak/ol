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

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_texture_mirrored_repeat

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_mirrored_repeat (gl:QueryExtension "GL_ARB_texture_mirrored_repeat"))

))
