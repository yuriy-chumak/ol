; ===========================================================================
; ARB_texture_non_power_of_two                       (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_non_power_of_two.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_non_power_of_two)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_texture_non_power_of_two

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_non_power_of_two (gl:QueryExtension "GL_ARB_texture_non_power_of_two"))

))
