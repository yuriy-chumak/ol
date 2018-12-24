; ===========================================================================
; ARB_window_pos                                     (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_window_pos.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB window_pos)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_window_pos

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_window_pos (gl:QueryExtension "GL_ARB_window_pos"))

))
