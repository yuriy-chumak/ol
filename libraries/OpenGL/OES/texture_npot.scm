; ===========================================================================
; OES_texture_npot
;
;  https://www.khronos.org/registry/OpenGL/extensions/OES/OES_texture_npot.txt
;
; Version
;  Last Modifed Date: 2011-03-07
;  Author Revision: 3
;
; Overview
;
(define-library (OpenGL OES texture_npot)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL ES platform))

; ---------------------------------------------------------------------------
(export OES_texture_npot

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define OES_texture_npot (gl:QueryExtension "GL_OES_texture_npot"))
))
