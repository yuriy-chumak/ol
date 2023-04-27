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

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  None

; ---------------------------------------------------------------------------
(export ARB_texture_cube_map

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_NORMAL_MAP_ARB
   GL_REFLECTION_MAP_ARB

   GL_TEXTURE_CUBE_MAP_ARB
   GL_TEXTURE_BINDING_CUBE_MAP_ARB

   GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB

   GL_PROXY_TEXTURE_CUBE_MAP_ARB

   GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_cube_map (gl:QueryExtension "GL_ARB_texture_cube_map"))

   (define GL_NORMAL_MAP_ARB                      #x8511)
   (define GL_REFLECTION_MAP_ARB                  #x8512)
   (define GL_TEXTURE_CUBE_MAP_ARB                #x8513)
   (define GL_TEXTURE_BINDING_CUBE_MAP_ARB        #x8514)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB     #x8515)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB     #x8516)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB     #x8517)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB     #x8518)
   (define GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB     #x8519)
   (define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB     #x851A)
   (define GL_PROXY_TEXTURE_CUBE_MAP_ARB          #x851B)
   (define GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB       #x851C)

))
