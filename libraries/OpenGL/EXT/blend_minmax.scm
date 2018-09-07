; ===========================================================================
; EXT_blend_minmax                                   (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_minmax.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_minmax)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))
      
; ---------------------------------------------------------------------------
(export EXT_blend_minmax
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_minmax (gl:QueryExtension "GL_EXT_blend_minmax"))

))
