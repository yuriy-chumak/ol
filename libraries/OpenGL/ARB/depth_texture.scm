; ===========================================================================
; ARB_depth_texture                                  (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_depth_texture.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB depth_texture)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1))

; ---------------------------------------------------------------------------
(export ARB_depth_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_DEPTH_COMPONENT
   GL_DEPTH_COMPONENT16_ARB
   GL_DEPTH_COMPONENT24_ARB
   GL_DEPTH_COMPONENT32_ARB

   GL_DEPTH_COMPONENT

   GL_TEXTURE_DEPTH_SIZE_ARB
   GL_DEPTH_TEXTURE_MODE_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_depth_texture (gl:QueryExtension "GL_ARB_depth_texture"))

   (define GL_DEPTH_COMPONENT16_ARB       #x81A5) ;(same as DEPTH_COMPONENT16_SGIX)
   (define GL_DEPTH_COMPONENT24_ARB       #x81A6) ;(same as DEPTH_COMPONENT24_SGIX)
   (define GL_DEPTH_COMPONENT32_ARB       #x81A7) ;(same as DEPTH_COMPONENT32_SGIX)
   (define GL_TEXTURE_DEPTH_SIZE_ARB      #x884A)
   (define GL_DEPTH_TEXTURE_MODE_ARB      #x884B)

))
