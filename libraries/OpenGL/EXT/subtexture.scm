; ===========================================================================
; EXT_subtexture                                     (included in OpenGL 1.1)
;
;	Various methods to alter texture images, including image copying and sub-image replacement.
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_subtexture.txt
;
; Version
;	$Date: 1995/10/03 05:39:55 $ $Revision: 1.17 $
;
; Overview
;	This extension allows a contiguous portion of an already-existing
;	texture image to be redefined, without affecting the remaining portion
;	of the image, or any of the other state that describe the texture.  No
;	provision is made to query a subregion of a texture.

;	Semantics for null image pointers are defined for TexImage1D,
;	TexImage2D, and TexImage3DEXT.  Null image pointers can be used by
;	applications to effectively support texture arrays whose dimensions
;	are not a power of 2.
(define-library (OpenGL EXT subtexture)

; ---------------------------------------------------------------------------
; Dependencies
;	EXT_abgr affects the definition of this extension
;	EXT_texture3D affects the definition of this extension
(import (scheme core) (OpenGL))

;	EXT_texture is required.
(import (OpenGL EXT texture))

; ---------------------------------------------------------------------------
   (export
    EXT_subtexture

; ---------------------------------------------------------------------------
; New Procedures and Functions

   ;TexSubImage1DEXT
   ;TexSubImage2DEXT
   ;TexSubImage3DEXT

; ---------------------------------------------------------------------------
; New Tokens

   ;none

)

; ---------------------------------------------------------------------------
   (begin
;   (gl:make-current)
   (define EXT_subtexture (gl:ExtensionSupported? "GL_EXT_subtexture"))

;   (gl:stop-current)
))
