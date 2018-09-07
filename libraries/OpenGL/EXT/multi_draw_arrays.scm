; ===========================================================================
; EXT_multi_drtaw_arrays                             (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_multi_drtaw_arrays.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT multi_draw_arrays)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))
      
; ---------------------------------------------------------------------------
(export EXT_multi_draw_arrays
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define EXT_multi_draw_arrays (gl:QueryExtension "GL_EXT_multi_draw_arrays"))

))
