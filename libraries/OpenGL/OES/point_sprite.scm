; ===========================================================================
; OES_point_sprite                                (included in OpenGL ES 1.1)
;
;  https://www.khronos.org/registry/OpenGL/extensions/OES/OES_point_sprite.txt
;
; Version
;  Last Modified Date: August 5, 2004
;
; Overview
;
(define-library (OpenGL OES point_sprite)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL ES platform))

; ---------------------------------------------------------------------------
(export OES_point_sprite

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
   POINT_SPRITE_OES
   COORD_REPLACE_OES
)

; ---------------------------------------------------------------------------
(begin
   (define OES_point_sprite (gl:QueryExtension "GL_OES_point_sprite"))

   (define POINT_SPRITE_OES  #x8861)
   (define COORD_REPLACE_OES #x8862)
))
