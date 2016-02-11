; EXT_texture
;	Various texturing improvements, including proxy textures and sized internal formats.
;
;	http://oss.sgi.com/projects/ogl-sample/registry/EXT/texture.txt
;	https://www.opengl.org/registry/specs/EXT/texture.txt
;
; Version
;	$Date: 1996/04/05 19:17:03 $ $Revision: 1.21 $
;
; Overview
;	The original intention of this extension was simply to support various
;	numeric resolutions of color components in texture images.  While it
;	accomplishes this, it also accomplishes a larger task, that of
;	formalizing the notion of an internal format for images, corresponding
;	to the external format that already existed for image data in host
;	memory.  This notion of an internal image format will be used
;	extensively in later extensions, especially those concerned with pixel
;	manipulation.
;
;	The idea of an internal format is simple: rather than treating a
;	retained image as having 1, 2, 3, or 4 components, treat it as though
;	it has a specific format, such as LUMINANCE_ALPHA, or just ALPHA.  Then
;	define the semantics of the use of internal images with these formats in
;	a consistent way.  Because texture mapping is already defined in GL, the
;	semantics for internal-format images were chosen to match those of the 1,
;	2, 3, and 4 component internal images that already existed.  The new
;	semantics are a superset of the old ones, however, so this extension
;	adds capabilities to GL, as well as allowing internal resolutions to be
;	specified.
;
;	This extension also defines a robust method for applications to
;	determine what combinations of texture dimensions and resolutions are
;	supported by an implementation.  It also introduces a new texture
;	environment: REPLACE_EXT.
(define-library (OpenGL EXT texture)

; ---------------------------------------------------------------------------
; Dependencies
;	None
   (import
      (r5rs base) (owl io)
      (OpenGL version-1-0))
      
; ---------------------------------------------------------------------------
   (export
    EXT_texture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions
   
; ---------------------------------------------------------------------------
; New Tokens
    
)
  
; ---------------------------------------------------------------------------
   (begin
;   (gl:make-current)
   (define EXT_texture (glIsExtensionSupported "GL_EXT_texture"))
   
;   (gl:stop-current)
))
