; ===========================================================================
; ARB_clear_texture                                     (required OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_clear_texture.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB clear_texture)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; ---------------------------------------------------------------------------
(export ARB_clear_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions
   glClearTexImage
   glClearTexSubImage
; ---------------------------------------------------------------------------
; New Tokens
   GL_CLEAR_TEXTURE
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_clear_texture (gl:QueryExtension "GL_ARB_clear_texture"))

   (define glClearTexImage (if ARB_clear_texture
      (gl:GetProcAddress GLvoid "glClearTexImage" GLuint GLint GLenum GLenum fft-any)))
   (define glClearTexSubImage (if ARB_clear_texture
      (gl:GetProcAddress GLvoid "ARB_clear_texture" GLuint GLint GLint GLint GLint GLsizei GLsizei GLsizei GLenum GLenum fft-any)))

   (define GL_CLEAR_TEXTURE #x9365)
))
