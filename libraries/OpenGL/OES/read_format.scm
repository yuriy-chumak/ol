; ===========================================================================
; OES_read_format                                 (included in OpenGL ES 1.1)
;
;  https://www.khronos.org/registry/OpenGL/extensions/OES/OES_read_format.txt
;
; Version
;  Last Modifed Date: Jan 4, 2006
;  Author Revision: 0.3
;
; Overview
;
(define-library (OpenGL OES read_format)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL ES platform))

; ---------------------------------------------------------------------------
(export OES_read_format

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
   IMPLEMENTATION_COLOR_READ_TYPE_OES
   IMPLEMENTATION_COLOR_READ_FORMAT_OES
)

; ---------------------------------------------------------------------------
(begin
   (define OES_read_format (gl:QueryExtension "GL_OES_read_format"))

   (define IMPLEMENTATION_COLOR_READ_TYPE_OES   #x8B9A)
   (define IMPLEMENTATION_COLOR_READ_FORMAT_OES #x8B9B)
))
