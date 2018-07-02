;
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
      (scheme core) (owl io)
      (OpenGL version-1-0))
      
; ---------------------------------------------------------------------------
   (export
    EXT_texture
    
; ---------------------------------------------------------------------------
; New Procedures and Functions

   ;none   

; ---------------------------------------------------------------------------
; New Tokens

;    Accepted by the <components> parameter of TexImage1D and TexImage2D:
;
;        ALPHA4_EXT                       0x803B
;        ALPHA8_EXT                       0x803C
;        ALPHA12_EXT                      0x803D
;        ALPHA16_EXT                      0x803E
;        LUMINANCE4_EXT                   0x803F
;        LUMINANCE8_EXT                   0x8040
;        LUMINANCE12_EXT                  0x8041
;        LUMINANCE16_EXT                  0x8042
;        LUMINANCE4_ALPHA4_EXT            0x8043
;        LUMINANCE6_ALPHA2_EXT            0x8044
;        LUMINANCE8_ALPHA8_EXT            0x8045
;        LUMINANCE12_ALPHA4_EXT           0x8046
;        LUMINANCE12_ALPHA12_EXT          0x8047
;        LUMINANCE16_ALPHA16_EXT          0x8048
;        INTENSITY_EXT                    0x8049
;        INTENSITY4_EXT                   0x804A
;        INTENSITY8_EXT                   0x804B
;        INTENSITY12_EXT                  0x804C
;        INTENSITY16_EXT                  0x804D
;        RGB2_EXT                         0x804E
;        RGB4_EXT                         0x804F
;        RGB5_EXT                         0x8050
;        RGB8_EXT                         0x8051
;        RGB10_EXT                        0x8052
;        RGB12_EXT                        0x8053
;        RGB16_EXT                        0x8054
;        RGBA2_EXT                        0x8055
;        RGBA4_EXT                        0x8056
;        RGB5_A1_EXT                      0x8057
;        RGBA8_EXT                        0x8058
;        RGB10_A2_EXT                     0x8059
;        RGBA12_EXT                       0x805A
;        RGBA16_EXT                       0x805B
;
;    Accepted by the <pname> parameters of GetTexLevelParameteriv and
;    GetTexLevelParameterfv:
;
;        TEXTURE_RED_SIZE_EXT             0x805C
;        TEXTURE_GREEN_SIZE_EXT           0x805D
;        TEXTURE_BLUE_SIZE_EXT            0x805E
;        TEXTURE_ALPHA_SIZE_EXT           0x805F
;        TEXTURE_LUMINANCE_SIZE_EXT       0x8060
;        TEXTURE_INTENSITY_SIZE_EXT       0x8061
;
;    Accepted by the <params> parameter of TexEnvf, TexEnvi, TexEnvfv, and
;    TexEnvfi when the <pname> parameter value is
;    GL_TEXTURE_ENV_MODE
;
;        REPLACE_EXT                      0x8062
;
;    Accepted by the <target> parameters of TexImage1D,
;    GetTexLevelParameteriv, and GetTexLevelParameterfv:
;
;        PROXY_TEXTURE_1D_EXT             0x8063
;
;    Accepted by the <target> parameters of TexImage2D,
;    GetTexLevelParameteriv, and GetTexLevelParameterfv:
;
;        PROXY_TEXTURE_2D_EXT             0x8064
;
;Additions to Chapter 2 of the GL Specification (OpenGL Operation)
;
;    A new error token value, TEXTURE_TOO_LARGE_EXT, is defined and added to
;    table 2.3:
;
;
;        TEXTURE_TOO_LARGE_EXT            0x8065

    
)
  
; ---------------------------------------------------------------------------
   (begin
;   (gl:make-current)
   (define EXT_texture (gl:ExtensionSupported? "GL_EXT_texture"))
   
;   (gl:stop-current)
))
