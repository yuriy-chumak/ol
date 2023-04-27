; ===========================================================================
; ARB_texture_env_combine                            (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_env_combine.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_env_combine)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  
(import
   (OpenGL 1.1)
   (OpenGL ARB multitexture)
)

; ---------------------------------------------------------------------------
(export ARB_texture_env_combine

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

   GL_COMBINE_RGB_ARB
   GL_COMBINE_ALPHA_ARB
   GL_SOURCE0_RGB_ARB
   GL_SOURCE1_RGB_ARB
   GL_SOURCE2_RGB_ARB
   GL_SOURCE0_ALPHA_ARB
   GL_SOURCE1_ALPHA_ARB
   GL_SOURCE2_ALPHA_ARB
   GL_OPERAND0_RGB_ARB
   GL_OPERAND1_RGB_ARB
   GL_OPERAND2_RGB_ARB
   GL_OPERAND0_ALPHA_ARB
   GL_OPERAND1_ALPHA_ARB
   GL_OPERAND2_ALPHA_ARB
   GL_RGB_SCALE_ARB
   GL_ALPHA_SCALE

   GL_REPLACE
   GL_MODULATE
   GL_ADD
   GL_ADD_SIGNED_ARB
   GL_INTERPOLATE_ARB
   GL_SUBTRACT_ARB

   GL_TEXTURE
   GL_CONSTANT_ARB
   GL_PRIMARY_COLOR_ARB
   GL_PREVIOUS_ARB

   GL_SRC_COLOR
   GL_ONE_MINUS_SRC_COLOR
   GL_SRC_ALPHA
   GL_ONE_MINUS_SRC_ALPHA     

   GL_SRC_ALPHA
   GL_ONE_MINUS_SRC_ALPHA

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_env_combine (gl:QueryExtension "GL_ARB_texture_env_combine"))

   (define GL_COMBINE_RGB_ARB                                 #x8571)
   (define GL_COMBINE_ALPHA_ARB                               #x8572)
   (define GL_SOURCE0_RGB_ARB                                 #x8580)
   (define GL_SOURCE1_RGB_ARB                                 #x8581)
   (define GL_SOURCE2_RGB_ARB                                 #x8582)
   (define GL_SOURCE0_ALPHA_ARB                               #x8588)
   (define GL_SOURCE1_ALPHA_ARB                               #x8589)
   (define GL_SOURCE2_ALPHA_ARB                               #x858A)
   (define GL_OPERAND0_RGB_ARB                                #x8590)
   (define GL_OPERAND1_RGB_ARB                                #x8591)
   (define GL_OPERAND2_RGB_ARB                                #x8592)
   (define GL_OPERAND0_ALPHA_ARB                              #x8598)
   (define GL_OPERAND1_ALPHA_ARB                              #x8599)
   (define GL_OPERAND2_ALPHA_ARB                              #x859A)
   (define GL_RGB_SCALE_ARB                                   #x8573)
   (define GL_ADD_SIGNED_ARB                                  #x8574)
   (define GL_INTERPOLATE_ARB                                 #x8575)
   (define GL_SUBTRACT_ARB                                    #x84E7)
   (define GL_CONSTANT_ARB                                    #x8576)
   (define GL_PRIMARY_COLOR_ARB                               #x8577)
   (define GL_PREVIOUS_ARB                                    #x8578)

))
