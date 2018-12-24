; ===========================================================================
; ARB_transpose_matrix                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_transpose_matrix.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB transpose_matrix)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_transpose_matrix

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_transpose_matrix (gl:QueryExtension "GL_ARB_transpose_matrix"))

))
