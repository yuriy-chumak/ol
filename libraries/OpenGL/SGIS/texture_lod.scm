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

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL EXT texture))

; ---------------------------------------------------------------------------
(export SGIS_texture_lod

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_TEXTURE_MIN_LOD_SGIS
   GL_TEXTURE_MAX_LOD_SGIS
   GL_TEXTURE_BASE_LEVEL_SGIS
   GL_TEXTURE_MAX_LEVEL_SGIS

)

; ---------------------------------------------------------------------------
(begin
   (define SGIS_texture_lod (gl:QueryExtension "GL_SGIS_texture_lod"))

   (define GL_TEXTURE_MIN_LOD_SGIS            #x813A)
   (define GL_TEXTURE_MAX_LOD_SGIS            #x813B)
   (define GL_TEXTURE_BASE_LEVEL_SGIS         #x813C)
   (define GL_TEXTURE_MAX_LEVEL_SGIS          #x813D)

))
