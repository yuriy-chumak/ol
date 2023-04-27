; ===========================================================================
; EXT_blend_func_separate                            (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_func_separate.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT blend_func_separate)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
; None

; ---------------------------------------------------------------------------
(export EXT_blend_func_separate

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glBlendFuncSeparateEXT

; ---------------------------------------------------------------------------
; New Tokens

   GL_BLEND_DST_RGB_EXT
   GL_BLEND_SRC_RGB_EXT
   GL_BLEND_DST_ALPHA_EXT
   GL_BLEND_SRC_ALPHA_EXT
)

; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_func_separate (gl:QueryExtension "GL_EXT_blend_func_separate"))

   (define GL gl:GetProcAddress)
   (define glBlendFuncSeparateEXT (GL GLvoid "BlendFuncSeparateEXT" GLenum GLenum GLenum GLenum))

   (define GL_BLEND_DST_RGB_EXT                  #x80C8)
   (define GL_BLEND_SRC_RGB_EXT                  #x80C9)
   (define GL_BLEND_DST_ALPHA_EXT                #x80CA)
   (define GL_BLEND_SRC_ALPHA_EXT                #x80CB)

))
