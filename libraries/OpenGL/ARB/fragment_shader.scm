; ===========================================================================
; ARB_fragment_shader                                (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_fragment_shader.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB fragment_shader)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.0)
   (OpenGL ARB shader_objects))

; ---------------------------------------------------------------------------
(export ARB_fragment_shader

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_FRAGMENT_SHADER_ARB

   GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB
   GL_MAX_TEXTURE_COORDS_ARB
   GL_MAX_TEXTURE_IMAGE_UNITS_ARB

   GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_fragment_shader (gl:QueryExtension "GL_ARB_fragment_shader"))

   (define GL_FRAGMENT_SHADER_ARB                             #x8B30)
   (define GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB             #x8B49)
   (define GL_MAX_TEXTURE_COORDS_ARB                          #x8871)
   (define GL_MAX_TEXTURE_IMAGE_UNITS_ARB                     #x8872)
   (define GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB             #x8B8B)
))
