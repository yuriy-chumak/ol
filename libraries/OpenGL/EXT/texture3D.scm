; ===========================================================================
; EXT_texture3D                                      (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture3D.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT texture3D)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_texture3D

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture3D (gl:QueryExtension "GL_EXT_texture3D"))

))
