; ===========================================================================
; EXT_texture                                        (included in OpenGL 1.1)
;
;    Various texturing improvements, including proxy textures and sized internal formats.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture.txt
;
; Version
;    $Date: 1996/04/05 19:17:03 $ $Revision: 1.21 $
;
; Overview
;    The original intention of this extension was simply to support various
;    numeric resolutions of color components in texture images.  While it
;    accomplishes this, it also accomplishes a larger task, that of
;    formalizing the notion of an internal format for images, corresponding
;    to the external format that already existed for image data in host
;    memory.  This notion of an internal image format will be used
;    extensively in later extensions, especially those concerned with pixel
;    manipulation.
;
;    The idea of an internal format is simple: rather than treating a
;    retained image as having 1, 2, 3, or 4 components, treat it as though
;    it has a specific format, such as LUMINANCE_ALPHA, or just ALPHA.  Then
;    define the semantics of the use of internal images with these formats in
;    a consistent way.  Because texture mapping is already defined in GL, the
;    semantics for internal-format images were chosen to match those of the 1,
;    2, 3, and 4 component internal images that already existed.  The new
;    semantics are a superset of the old ones, however, so this extension
;    adds capabilities to GL, as well as allowing internal resolutions to be
;    specified.
;
;    This extension also defines a robust method for applications to
;    determine what combinations of texture dimensions and resolutions are
;    supported by an implementation.  It also introduces a new texture
;    environment: REPLACE_EXT.
(define-library (OpenGL EXT texture)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;	  None

; ---------------------------------------------------------------------------
(export EXT_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions
;    None

; ---------------------------------------------------------------------------
; New Tokens

   GL_ALPHA4_EXT
   GL_ALPHA8_EXT
   GL_ALPHA12_EXT
   GL_ALPHA16_EXT
   GL_LUMINANCE4_EXT
   GL_LUMINANCE8_EXT
   GL_LUMINANCE12_EXT
   GL_LUMINANCE16_EXT
   GL_LUMINANCE4_ALPHA4_EXT
   GL_LUMINANCE6_ALPHA2_EXT
   GL_LUMINANCE8_ALPHA8_EXT
   GL_LUMINANCE12_ALPHA4_EXT
   GL_LUMINANCE12_ALPHA12_EXT
   GL_LUMINANCE16_ALPHA16_EXT
   GL_INTENSITY_EXT
   GL_INTENSITY4_EXT
   GL_INTENSITY8_EXT
   GL_INTENSITY12_EXT
   GL_INTENSITY16_EXT
   GL_RGB2_EXT
   GL_RGB4_EXT
   GL_RGB5_EXT
   GL_RGB8_EXT
   GL_RGB10_EXT
   GL_RGB12_EXT
   GL_RGB16_EXT
   GL_RGBA2_EXT
   GL_RGBA4_EXT
   GL_RGB5_A1_EXT
   GL_RGBA8_EXT
   GL_RGB10_A2_EXT
   GL_RGBA12_EXT
   GL_RGBA16_EXT

   GL_TEXTURE_RED_SIZE_EXT
   GL_TEXTURE_GREEN_SIZE_EXT
   GL_TEXTURE_BLUE_SIZE_EXT
   GL_TEXTURE_ALPHA_SIZE_EXT
   GL_TEXTURE_LUMINANCE_SIZE_EXT
   GL_TEXTURE_INTENSITY_SIZE_EXT

   GL_REPLACE_EXT
   GL_PROXY_TEXTURE_1D_EXT
   GL_PROXY_TEXTURE_2D_EXT
   GL_TEXTURE_TOO_LARGE_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture (gl:QueryExtension "GL_EXT_texture"))

   (define GL_ALPHA4_EXT                       #x803B)
   (define GL_ALPHA8_EXT                       #x803C)
   (define GL_ALPHA12_EXT                      #x803D)
   (define GL_ALPHA16_EXT                      #x803E)
   (define GL_LUMINANCE4_EXT                   #x803F)
   (define GL_LUMINANCE8_EXT                   #x8040)
   (define GL_LUMINANCE12_EXT                  #x8041)
   (define GL_LUMINANCE16_EXT                  #x8042)
   (define GL_LUMINANCE4_ALPHA4_EXT            #x8043)
   (define GL_LUMINANCE6_ALPHA2_EXT            #x8044)
   (define GL_LUMINANCE8_ALPHA8_EXT            #x8045)
   (define GL_LUMINANCE12_ALPHA4_EXT           #x8046)
   (define GL_LUMINANCE12_ALPHA12_EXT          #x8047)
   (define GL_LUMINANCE16_ALPHA16_EXT          #x8048)
   (define GL_INTENSITY_EXT                    #x8049)
   (define GL_INTENSITY4_EXT                   #x804A)
   (define GL_INTENSITY8_EXT                   #x804B)
   (define GL_INTENSITY12_EXT                  #x804C)
   (define GL_INTENSITY16_EXT                  #x804D)
   (define GL_RGB2_EXT                         #x804E)
   (define GL_RGB4_EXT                         #x804F)
   (define GL_RGB5_EXT                         #x8050)
   (define GL_RGB8_EXT                         #x8051)
   (define GL_RGB10_EXT                        #x8052)
   (define GL_RGB12_EXT                        #x8053)
   (define GL_RGB16_EXT                        #x8054)
   (define GL_RGBA2_EXT                        #x8055)
   (define GL_RGBA4_EXT                        #x8056)
   (define GL_RGB5_A1_EXT                      #x8057)
   (define GL_RGBA8_EXT                        #x8058)
   (define GL_RGB10_A2_EXT                     #x8059)
   (define GL_RGBA12_EXT                       #x805A)
   (define GL_RGBA16_EXT                       #x805B)

   (define GL_TEXTURE_RED_SIZE_EXT             #x805C)
   (define GL_TEXTURE_GREEN_SIZE_EXT           #x805D)
   (define GL_TEXTURE_BLUE_SIZE_EXT            #x805E)
   (define GL_TEXTURE_ALPHA_SIZE_EXT           #x805F)
   (define GL_TEXTURE_LUMINANCE_SIZE_EXT       #x8060)
   (define GL_TEXTURE_INTENSITY_SIZE_EXT       #x8061)

   (define GL_REPLACE_EXT                      #x8062)
   (define GL_PROXY_TEXTURE_1D_EXT             #x8063)
   (define GL_PROXY_TEXTURE_2D_EXT             #x8064)
   (define GL_TEXTURE_TOO_LARGE_EXT            #x8065)

))
