; ===========================================================================
; SGIS_generate_mipmap                                (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/SGIS/SGIS_generate_mipmap.txt
;
; Version
;
; Overview
;
(define-library (OpenGL SGIS generate_mipmap)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export SGIS_generate_mipmap
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

)
  
; ---------------------------------------------------------------------------
(begin
   (define SGIS_generate_mipmap (gl:QueryExtension "GL_SGIS_generate_mipmap"))

))
