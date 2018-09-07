; ===========================================================================
; EXT_draw_range_elements                            (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_polygon_offset.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT draw_range_elements)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export EXT_draw_range_elements
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
    
)
   
; ---------------------------------------------------------------------------
(begin
   (define EXT_draw_range_elements (gl:QueryExtension "GL_EXT_draw_range_elements"))

))
