; ===========================================================================
; EXT_copy_texture                                   (included in OpenGL 1.1)
;
;    Various methods to alter texture images, including image copying and sub-image replacement.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_copy_texture.txt
;
; Version
;    $Date: 1995/06/17 03:33:42 $ $Revision: 1.21 $
;
; Overview
;    This extension defines methods to load texture images directly from the
;    framebuffer.  Methods are defined for both complete and partial
;    replacement of a texture image.  Because it is not possible to define
;    an entire 3D texture using a 2D framebuffer image, 3D textures are
;    supported only for partial replacement.
(define-library (OpenGL EXT copy_texture)

; ---------------------------------------------------------------------------
; Dependencies
;	  EXT_texture3D affects the definition of this extension.
;	  SGIS_texture_filter4 affects the definition of this extension.
;	  EXT_subtexture affects the definition of this extension.
(import (scheme core)
        (OpenGL platform))

;    EXT_texture is required.
(import (OpenGL EXT texture))

; ---------------------------------------------------------------------------
(export EXT_copy_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glCopyTexImage1DEXT
   glCopyTexImage2DEXT
   glCopyTexSubImage1DEXT
   glCopyTexSubImage2DEXT
   glCopyTexSubImage3DEXT

; ---------------------------------------------------------------------------
; New Tokens
;    None

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_copy_texture (gl:QueryExtension "GL_EXT_copy_texture"))

   (setq GL GL_LIBRARY)
   (define glCopyTexImage1DEXT (GL GLvoid "glCopyTexImage1DEXT" GLenum GLint GLenum GLint GLint GLsizei GLint))
   (define glCopyTexImage2DEXT (GL GLvoid "glCopyTexImage2DEXT" GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint))
   (define glCopyTexSubImage1DEXT (GL GLvoid "glCopyTexSubImage1DEXT" GLenum GLint GLint GLint GLint GLsizei))
   (define glCopyTexSubImage2DEXT (GL GLvoid "glCopyTexSubImage2DEXT" GLenum GLint GLint GLint GLint GLint GLsizei GLsizei))
   (define glCopyTexSubImage3DEXT (GL GLvoid "glCopyTexSubImage3DEXT" GLenum GLint GLint GLint GLint GLint GLint GLsizei GLsizei))
))
