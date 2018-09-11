; OpenGL 2.1 (2006)

(define-library (OpenGL version-2-1)
(export

   GL_VERSION_2_1

   GL_PIXEL_PACK_BUFFER
   GL_PIXEL_UNPACK_BUFFER
   GL_PIXEL_PACK_BUFFER_BINDING
   GL_PIXEL_UNPACK_BUFFER_BINDING
   GL_FLOAT_MAT2x3
   GL_FLOAT_MAT2x4
   GL_FLOAT_MAT3x2
   GL_FLOAT_MAT3x4
   GL_FLOAT_MAT4x2
   GL_FLOAT_MAT4x3
   GL_SRGB
   GL_SRGB8
   GL_SRGB_ALPHA
   GL_SRGB8_ALPHA8
   GL_COMPRESSED_SRGB
   GL_COMPRESSED_SRGB_ALPHA
   GL_CURRENT_RASTER_SECONDARY_COLOR
   GL_SLUMINANCE_ALPHA
   GL_SLUMINANCE8_ALPHA8
   GL_SLUMINANCE
   GL_SLUMINANCE8
   GL_COMPRESSED_SLUMINANCE
   GL_COMPRESSED_SLUMINANCE_ALPHA

   glUniformMatrix2x3fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
   glUniformMatrix3x2fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
   glUniformMatrix2x4fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
   glUniformMatrix4x2fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
   glUniformMatrix3x4fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
   glUniformMatrix4x3fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)

   (exports (OpenGL version-2-0)))

(import (scheme core)
   (OpenGL version-2-0))

(begin
   (define GL_VERSION_2_1 1)

   (define GL_PIXEL_PACK_BUFFER              #x88EB)
   (define GL_PIXEL_UNPACK_BUFFER            #x88EC)
   (define GL_PIXEL_PACK_BUFFER_BINDING      #x88ED)
   (define GL_PIXEL_UNPACK_BUFFER_BINDING    #x88EF)
   (define GL_FLOAT_MAT2x3                   #x8B65)
   (define GL_FLOAT_MAT2x4                   #x8B66)
   (define GL_FLOAT_MAT3x2                   #x8B67)
   (define GL_FLOAT_MAT3x4                   #x8B68)
   (define GL_FLOAT_MAT4x2                   #x8B69)
   (define GL_FLOAT_MAT4x3                   #x8B6A)
   (define GL_SRGB                           #x8C40)
   (define GL_SRGB8                          #x8C41)
   (define GL_SRGB_ALPHA                     #x8C42)
   (define GL_SRGB8_ALPHA8                   #x8C43)
   (define GL_COMPRESSED_SRGB                #x8C48)
   (define GL_COMPRESSED_SRGB_ALPHA          #x8C49)
   (define GL_CURRENT_RASTER_SECONDARY_COLOR #x845F)
   (define GL_SLUMINANCE_ALPHA               #x8C44)
   (define GL_SLUMINANCE8_ALPHA8             #x8C45)
   (define GL_SLUMINANCE                     #x8C46)
   (define GL_SLUMINANCE8                    #x8C47)
   (define GL_COMPRESSED_SLUMINANCE          #x8C4A)
   (define GL_COMPRESSED_SLUMINANCE_ALPHA    #x8C4B)

   (define glUniformMatrix2x3fv (GL GLvoid "glUniformMatrix2x3fv" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix3x2fv (GL GLvoid "glUniformMatrix3x2fv" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix2x4fv (GL GLvoid "glUniformMatrix2x4fv" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix4x2fv (GL GLvoid "glUniformMatrix4x2fv" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix3x4fv (GL GLvoid "glUniformMatrix3x4fv" GLint GLsizei GLboolean GLfloat*))
   (define glUniformMatrix4x3fv (GL GLvoid "glUniformMatrix4x3fv" GLint GLsizei GLboolean GLfloat*))

   ; notes:
   ; These are useful extensions when targeting GL 2.1 hardware.
   ;; GL_ARB_vertex_array_object
   ;; GL_ARB_framebuffer_object
   ;;      If not this, you get very similar functionality from these:
   ;;      GL_EXT_framebuffer_object
   ;;      GL_EXT_framebuffer_blit
   ;;      GL_EXT_framebuffer_multisample
   ;;      GL_EXT_packed_depth_stencil.
   ;;  GL_ARB_map_buffer_range
   ;;  GL_ARB_copy_buffer
   ;;  GL_ARB_texture_rectangle
   ;;  GL_ARB_color_buffer_float
   ;;  GL_ARB_half_float_pixel
   ;;  GL_ARB_sync


))
