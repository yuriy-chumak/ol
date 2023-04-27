; ===========================================================================
; EXT_separate_specular_color                        (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_separate_specular_color.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT separate_specular_color)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  None

; ---------------------------------------------------------------------------
(export EXT_separate_specular_color

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_LIGHT_MODEL_COLOR_CONTROL_EXT
   GL_SINGLE_COLOR_EXT
   GL_SEPARATE_SPECULAR_COLOR_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_separate_specular_color (gl:QueryExtension "GL_EXT_separate_specular_color"))

   (define GL_LIGHT_MODEL_COLOR_CONTROL_EXT	#x81F8)
   (define GL_SINGLE_COLOR_EXT			      #x81F9)
   (define GL_SEPARATE_SPECULAR_COLOR_EXT		#x81FA)

))
