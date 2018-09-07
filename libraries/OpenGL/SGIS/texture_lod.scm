; ===========================================================================
; SGIS_texture_lod                                   (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/SGIS/SGIS_texture_lod.txt
;
; Version
;
; Overview
;
(define-library (OpenGL SGIS texture_lod)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))
      
; ---------------------------------------------------------------------------
(export SGIS_texture_lod
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define SGIS_texture_lod (gl:QueryExtension "GL_SGIS_texture_lod"))

))
