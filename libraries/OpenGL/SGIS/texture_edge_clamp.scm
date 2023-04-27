; ===========================================================================
; SGIS_texture_edge_clamp                            (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/SGIS/SGIS_texture_edge_clamp.txt
;
; Version
;
; Overview
;
(define-library (OpenGL SGIS texture_edge_clamp)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
(export SGIS_texture_edge_clamp

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_CLAMP_TO_EDGE_SGIS

)

; ---------------------------------------------------------------------------
(begin
   (define SGIS_texture_edge_clamp (gl:QueryExtension "GL_SGIS_texture_edge_clamp"))

   (define GL_CLAMP_TO_EDGE_SGIS	  #x812F)

))
