; ===========================================================================
; ARB_depth_texture                                  (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_depth_texture.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB depth_texture)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_depth_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_depth_texture (gl:QueryExtension "GL_ARB_depth_texture"))

))
