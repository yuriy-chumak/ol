; ===========================================================================
; ARB_vertex_buffer_object                           (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_vertex_buffer_object.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB vertex_buffer_object)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_vertex_buffer_object

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_vertex_buffer_object (gl:QueryExtension "GL_ARB_vertex_buffer_object"))

))
