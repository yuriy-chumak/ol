; ===========================================================================
; EXT_blend_subtract                                 (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_subtract.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_subtract)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))
      
; ---------------------------------------------------------------------------
(export EXT_blend_subtract
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_subtract (gl:QueryExtension "GL_EXT_blend_subtract"))

))
