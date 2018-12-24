; ===========================================================================
; ARB_fragment_shader                                (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_fragment_shader.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB fragment_shader)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_fragment_shader

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_fragment_shader (gl:QueryExtension "GL_ARB_fragment_shader"))

))
