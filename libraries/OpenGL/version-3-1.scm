; OpenGL 3.1 (24 Mar 2009) GLSL 1.4
(define-library (OpenGL version-3-1)
(export
   GL_VERSION_3_1

   GL_RED_SNORM
   GL_RG_SNORM
   GL_RGB_SNORM
   GL_RGBA_SNORM
   GL_R8_SNORM
   GL_RG8_SNORM
   GL_RGB8_SNORM
   GL_RGBA8_SNORM
   GL_R16_SNORM
   GL_RG16_SNORM
   GL_RGB16_SNORM
   GL_RGBA16_SNORM
   GL_SIGNED_NORMALIZED
   GL_PRIMITIVE_RESTART
   GL_PRIMITIVE_RESTART_INDEX

   (exports (OpenGL version-3-0)))

(import (scheme core)
   (OpenGL version-3-0))

(begin
   (define GL_VERSION_3_1 1)

   (define GL_RED_SNORM                      #x8F90)
   (define GL_RG_SNORM                       #x8F91)
   (define GL_RGB_SNORM                      #x8F92)
   (define GL_RGBA_SNORM                     #x8F93)
   (define GL_R8_SNORM                       #x8F94)
   (define GL_RG8_SNORM                      #x8F95)
   (define GL_RGB8_SNORM                     #x8F96)
   (define GL_RGBA8_SNORM                    #x8F97)
   (define GL_R16_SNORM                      #x8F98)
   (define GL_RG16_SNORM                     #x8F99)
   (define GL_RGB16_SNORM                    #x8F9A)
   (define GL_RGBA16_SNORM                   #x8F9B)
   (define GL_SIGNED_NORMALIZED              #x8F9C)
   (define GL_PRIMITIVE_RESTART              #x8F9D)
   (define GL_PRIMITIVE_RESTART_INDEX        #x8F9E)

))