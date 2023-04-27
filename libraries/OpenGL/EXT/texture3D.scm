; ===========================================================================
; EXT_texture3D                                      (included in OpenGL 1.2)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_texture3D.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT texture3D)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;  EXT_abgr affects the definition of this extension
(import
   (OpenGL EXT texture))

; ---------------------------------------------------------------------------
(export EXT_texture3D

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glTexImage3DEXT

; ---------------------------------------------------------------------------
; New Tokens

   GL_PACK_SKIP_IMAGES_EXT
   GL_PACK_IMAGE_HEIGHT_EXT
   GL_UNPACK_SKIP_IMAGES_EXT
   GL_UNPACK_IMAGE_HEIGHT_EXT
   GL_TEXTURE_3D_EXT
   GL_PROXY_TEXTURE_3D_EXT
   GL_TEXTURE_DEPTH_EXT
   GL_TEXTURE_WRAP_R_EXT
   GL_MAX_3D_TEXTURE_SIZE_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_texture3D (gl:QueryExtension "GL_EXT_texture3D"))

   (setq GL gl:GetProcAddress)
   (define glTexImage3DEXT (GL GLvoid "glTexImage3DEXT" GLenum GLint GLenum GLsizei GLsizei GLsizei GLint GLenum GLenum fft-any))

   (define GL_PACK_SKIP_IMAGES_EXT             #x806B)
   (define GL_PACK_IMAGE_HEIGHT_EXT            #x806C)
   (define GL_UNPACK_SKIP_IMAGES_EXT           #x806D)
   (define GL_UNPACK_IMAGE_HEIGHT_EXT          #x806E)
   (define GL_TEXTURE_3D_EXT                   #x806F)
   (define GL_PROXY_TEXTURE_3D_EXT             #x8070)
   (define GL_TEXTURE_DEPTH_EXT                #x8071)
   (define GL_TEXTURE_WRAP_R_EXT               #x8072)
   (define GL_MAX_3D_TEXTURE_SIZE_EXT          #x8073)

))
