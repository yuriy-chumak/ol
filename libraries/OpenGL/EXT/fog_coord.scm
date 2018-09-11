; ===========================================================================
; EXT_fog_coord                                      (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_fog_coord.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT fog_coord)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_fog_coord

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_fog_coord (gl:QueryExtension "GL_EXT_fog_coord"))

))
