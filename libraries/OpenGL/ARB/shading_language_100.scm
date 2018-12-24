; ===========================================================================
; ARB_shading_language_100                           (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shading_language_100.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shading_language_100)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_shading_language_100

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_shading_language_100 (gl:QueryExtension "GL_ARB_shading_language_100"))

))
