; ===========================================================================
; ARB_occlusion_query                                (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_occlusion_query.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB occlusion_query)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_occlusion_query

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_occlusion_query (gl:QueryExtension "GL_ARB_occlusion_query"))

))
