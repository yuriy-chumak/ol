; ===========================================================================
; ARB_texture_rg                                     (included in OpenGL 3.0)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_rg.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_rg)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_texture_rg

; ---------------------------------------------------------------------------
; New Procedures and Functions

   ;None

; ---------------------------------------------------------------------------
; New Tokens

;; Accepted by the <internalFormat> parameter of TexImage1D, TexImage2D,
;; TexImage3D, CopyTexImage1D, and CopyTexImage2D:
   GL_R8
   GL_R16

   GL_RG8
   GL_RG16

   GL_R16F
   GL_R32F

   GL_RG16F
   GL_RG32F

   GL_R8I
   GL_R8UI
   GL_R16I
   GL_R16UI
   GL_R32I
   GL_R32UI

   GL_RG8I
   GL_RG8UI
   GL_RG16I
   GL_RG16UI
   GL_RG32I
   GL_RG32UI

   GL_RED
   GL_RG

   GL_COMPRESSED_RED
   GL_COMPRESSED_RG

;; Accepted by the <format> parameter of TexImage1D, TexImage2D,
;; TexImage3D, TexSubImage1D, TexSubImage2D, TexSubImage3D,
;; and ReadPixels:

   GL_RG
   GL_RG_INTEGER

;; Accepted by the <format> parameter of DrawPixels:

   GL_RG

;; Accepted by the <param> parameter of the TexParameter{if}*
;; functions when <pname> is DEPTH_TEXTURE_MODE:

   GL_RED

;; Accepted by the <format> parameter of GetTexImage:

   GL_RG
   GL_RG_INTEGER
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_rg (gl:QueryExtension "GL_ARB_texture_rg"))

   (define GL_R8                      #x8229)
   (define GL_R16                     #x822A)
   (define GL_RG8                     #x822B)
   (define GL_RG16                    #x822C)
   (define GL_R16F                    #x822D)
   (define GL_R32F                    #x822E)
   (define GL_RG16F                   #x822F)
   (define GL_RG32F                   #x8230)
   (define GL_R8I                     #x8231)
   (define GL_R8UI                    #x8232)
   (define GL_R16I                    #x8233)
   (define GL_R16UI                   #x8234)
   (define GL_R32I                    #x8235)
   (define GL_R32UI                   #x8236)
   (define GL_RG8I                    #x8237)
   (define GL_RG8UI                   #x8238)
   (define GL_RG16I                   #x8239)
   (define GL_RG16UI                  #x823A)
   (define GL_RG32I                   #x823B)
   (define GL_RG32UI                  #x823C)
   (define GL_RED                     #x1903)
   (define GL_RG                      #x8227)
   (define GL_COMPRESSED_RED          #x8225)
   (define GL_COMPRESSED_RG           #x8226)
   (define GL_RG_INTEGER              #x8228)

))
