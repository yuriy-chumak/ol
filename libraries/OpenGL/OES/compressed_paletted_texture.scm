; ===========================================================================
; OES_compressed_paletted_texture                 (included in OpenGL ES 1.1)
;
;  https://www.khronos.org/registry/OpenGL/extensions/OES/OES_compressed_paletted_texture.txt
;
; Version
;  Last Modifed Date: 12 November 2005
;  Author Revision: 0.6
;
; Overview
;
(define-library (OpenGL OES compressed_paletted_texture)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL ES platform))

; ---------------------------------------------------------------------------
(export OES_compressed_paletted_texture

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens
   PALETTE4_RGB8_OES
   PALETTE4_RGBA8_OES
   PALETTE4_R5_G6_B5_OES
   PALETTE4_RGBA4_OES
   PALETTE4_RGB5_A1_OES
   PALETTE8_RGB8_OES
   PALETTE8_RGBA8_OES
   PALETTE8_R5_G6_B5_OES
   PALETTE8_RGBA4_OES
   PALETTE8_RGB5_A1_OES
)

; ---------------------------------------------------------------------------
(begin
   (define OES_compressed_paletted_texture (gl:QueryExtension "GL_OES_compressed_paletted_texture"))

   (define PALETTE4_RGB8_OES         #x8B90)
   (define PALETTE4_RGBA8_OES        #x8B91)
   (define PALETTE4_R5_G6_B5_OES     #x8B92)
   (define PALETTE4_RGBA4_OES        #x8B93)
   (define PALETTE4_RGB5_A1_OES      #x8B94)
   (define PALETTE8_RGB8_OES         #x8B95)
   (define PALETTE8_RGBA8_OES        #x8B96)
   (define PALETTE8_R5_G6_B5_OES     #x8B97)
   (define PALETTE8_RGBA4_OES        #x8B98)
   (define PALETTE8_RGB5_A1_OES      #x8B99)
))
