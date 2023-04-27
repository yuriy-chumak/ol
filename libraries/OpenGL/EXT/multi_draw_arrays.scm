; ===========================================================================
; EXT_multi_drtaw_arrays                             (included in OpenGL 1.4)
;
;	https://registry.khronos.org/OpenGL/extensions/EXT/EXT_multi_draw_arrays.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT multi_draw_arrays)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1))

; ---------------------------------------------------------------------------
(export EXT_multi_draw_arrays

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glMultiDrawArraysEXT
   glMultiDrawElementsEXT

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_multi_draw_arrays (gl:QueryExtension "GL_EXT_multi_draw_arrays"))

   (define GL gl:GetProcAddress)
   (define glMultiDrawArraysEXT (GL GLvoid "glMultiDrawArraysEXT" GLenum GLint* (fft* GLsizei) GLsizei))
   (define glMultiDrawElementsEXT (GL GLvoid "glMultiDrawElementsEXT" GLenum (fft* GLsizei) GLenum fft-any GLsizei))

))
