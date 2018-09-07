; ===========================================================================
; EXT_separate_specular_color                        (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_separate_specular_color.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT separate_specular_color)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_separate_specular_color
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define EXT_separate_specular_color (gl:QueryExtension "GL_EXT_separate_specular_color"))

))
