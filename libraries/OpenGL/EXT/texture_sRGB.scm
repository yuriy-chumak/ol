; ===========================================================================
; EXT_texture_sRGB                                   (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture_sRGB.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT texture_sRGB)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))
      
; ---------------------------------------------------------------------------
(export EXT_texture_sRGB
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define EXT_texture_sRGB (gl:QueryExtension "GL_EXT_texture_sRGB"))

))
