; ===========================================================================
; ARB_pixel_buffer_object                            (included in OpenGL 2.1)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_pixel_buffer_object.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB pixel_buffer_object)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_pixel_buffer_object

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_pixel_buffer_object (gl:QueryExtension "GL_ARB_pixel_buffer_object"))

))
