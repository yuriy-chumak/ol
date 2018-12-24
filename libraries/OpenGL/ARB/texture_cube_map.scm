; ===========================================================================
; ARB_texture_cube_map                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_cube_map.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_cube_map)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_texture_cube_map

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_cube_map (gl:QueryExtension "GL_ARB_texture_cube_map"))

))
