; ===========================================================================
; EXT_secondary_color                                (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_secondary_color.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT secondary_color)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_secondary_color

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_secondary_color (gl:QueryExtension "GL_EXT_secondary_color"))

))
