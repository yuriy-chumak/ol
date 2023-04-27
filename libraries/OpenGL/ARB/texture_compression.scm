; ===========================================================================
; ARB_texture_compression                            (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_texture_compression.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB texture_compression)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1)
   (OpenGL ARB texture_cube_map))

; ---------------------------------------------------------------------------
(export ARB_texture_compression

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glCompressedTexImage3DARB
   glCompressedTexImage2DARB
   glCompressedTexImage1DARB
   glCompressedTexSubImage3DARB
   glCompressedTexSubImage2DARB
   glCompressedTexSubImage1DARB
   glGetCompressedTexImageARB

; ---------------------------------------------------------------------------
; New Tokens

   GL_COMPRESSED_ALPHA_ARB
   GL_COMPRESSED_LUMINANCE_ARB
   GL_COMPRESSED_LUMINANCE_ALPHA_ARB
   GL_COMPRESSED_INTENSITY_ARB
   GL_COMPRESSED_RGB_ARB
   GL_COMPRESSED_RGBA_ARB

   GL_TEXTURE_COMPRESSION_HINT_ARB

   GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB
   GL_TEXTURE_COMPRESSED_ARB

   GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB
   GL_COMPRESSED_TEXTURE_FORMATS_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_texture_compression (gl:QueryExtension "GL_ARB_texture_compression"))

   (setq GL gl:GetProcAddress)

   (define GL_COMPRESSED_ALPHA_ARB                            #x84E9)
   (define GL_COMPRESSED_LUMINANCE_ARB                        #x84EA)
   (define GL_COMPRESSED_LUMINANCE_ALPHA_ARB                  #x84EB)
   (define GL_COMPRESSED_INTENSITY_ARB                        #x84EC)
   (define GL_COMPRESSED_RGB_ARB                              #x84ED)
   (define GL_COMPRESSED_RGBA_ARB                             #x84EE)

   (define GL_TEXTURE_COMPRESSION_HINT_ARB                    #x84EF)

   (define GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB               #x86A0)
   (define GL_TEXTURE_COMPRESSED_ARB                          #x86A1)

   (define GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB              #x86A2)
   (define GL_COMPRESSED_TEXTURE_FORMATS_ARB                  #x86A3)

   (define glCompressedTexImage3DARB (GL GLvoid "glCompressedTexImage3DARB" GLenum GLint GLenum GLsizei GLsizei GLsizei GLint GLsizei fft-any))
   (define glCompressedTexImage2DARB (GL GLvoid "glCompressedTexImage2DARB" GLenum GLint GLenum GLsizei GLsizei GLint GLsizei fft-any))
   (define glCompressedTexImage1DARB (GL GLvoid "glCompressedTexImage1DARB" GLenum GLint GLenum GLsizei GLint GLsizei fft-any))
   (define glCompressedTexSubImage3DARB (GL GLvoid "glCompressedTexSubImage3DARB" GLenum GLint GLint GLint GLint GLsizei GLsizei GLsizei GLenum GLsizei fft-any))
   (define glCompressedTexSubImage2DARB (GL GLvoid "glCompressedTexSubImage2DARB" GLenum GLint GLint GLint GLsizei GLsizei GLenum GLsizei fft-any))
   (define glCompressedTexSubImage1DARB (GL GLvoid "glCompressedTexSubImage1DARB" GLenum GLint GLint GLsizei GLenum GLsizei fft-any))
   (define glGetCompressedTexImageARB (GL GLvoid "glGetCompressedTexImageARB" GLenum GLint fft-any))

))
