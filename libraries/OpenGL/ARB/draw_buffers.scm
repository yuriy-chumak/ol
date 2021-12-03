; ===========================================================================
; ARB_draw_buffers                                   (included in OpenGL 2.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_draw_buffers.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB draw_buffers)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_draw_buffers

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glDrawBuffers ; void (sizei n, const enum *bufs);

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_draw_buffers (gl:QueryExtension "GL_ARB_draw_buffers"))
   (setq GL gl:GetProcAddress)

   (define glDrawBuffers (GL GLvoid "glDrawBuffers" GLsizei (fft* GLenum)))
))
