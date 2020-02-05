; ===========================================================================
; EXT_subtexture                                     (included in OpenGL 1.1)
;
;    Various methods to alter texture images, including image copying and sub-image replacement.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_subtexture.txt
;
; Version
;    $Date: 1995/10/03 05:39:55 $ $Revision: 1.17 $
;
; Overview
;    This extension allows a contiguous portion of an already-existing
;    texture image to be redefined, without affecting the remaining portion
;    of the image, or any of the other state that describe the texture.  No
;    provision is made to query a subregion of a texture.
;
;    Semantics for null image pointers are defined for TexImage1D,
;    TexImage2D, and TexImage3DEXT.  Null image pointers can be used by
;    applications to effectively support texture arrays whose dimensions
;    are not a power of 2.
(define-library (OpenGL EXT subtexture)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_abgr affects the definition of this extension
;	EXT_texture3D affects the definition of this extension
(import (scheme core)
        (OpenGL platform))

;	EXT_texture is required.
(import (OpenGL EXT texture))

; ---------------------------------------------------------------------------
(export EXT_subtexture

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glTexSubImage1DEXT
   glTexSubImage2DEXT
   glTexSubImage3DEXT

; ---------------------------------------------------------------------------
; New Tokens
;    None

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_subtexture (gl:QueryExtension "GL_EXT_subtexture"))

   (setq GL GL_LIBRARY)
   (define glTexSubImage1DEXT (GL GLvoid "glTexSubImage1DEXT" GLenum GLint GLint GLsizei GLenum GLenum fft-any))
   (define glTexSubImage2DEXT (GL GLvoid "glTexSubImage2DEXT" GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum fft-any))
   (define glTexSubImage3DEXT (GL GLvoid "glTexSubImage3DEXT" GLenum GLint GLint GLint GLint GLsizei GLsizei GLsizei GLenum GLenum fft-any))

))
