; ===========================================================================
; ARB_vertex_buffer_object                           (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_vertex_buffer_object.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB vertex_buffer_object)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies

; ---------------------------------------------------------------------------
(export ARB_vertex_buffer_object

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glBindBufferARB
   glDeleteBuffersARB
   glGenBuffersARB
   glIsBufferARB

   glBufferDataARB
   glBufferSubDataARB
   glGetBufferSubDataARB

   glMapBufferARB
   glUnmapBufferARB

   glGetBufferParameterivARB
   glGetBufferPointervARB

; ---------------------------------------------------------------------------
; New Tokens

   GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH_ARB

   GL_ARRAY_BUFFER_ARB
   GL_ELEMENT_ARRAY_BUFFER_ARB

   GL_ARRAY_BUFFER_BINDING_ARB
   GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB
   GL_VERTEX_ARRAY_BUFFER_BINDING_ARB
   GL_NORMAL_ARRAY_BUFFER_BINDING_ARB
   GL_COLOR_ARRAY_BUFFER_BINDING_ARB
   GL_INDEX_ARRAY_BUFFER_BINDING_ARB
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB
   GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB

   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB

   GL_STREAM_DRAW_ARB
   GL_STREAM_READ_ARB
   GL_STREAM_COPY_ARB
   GL_STATIC_DRAW_ARB
   GL_STATIC_READ_ARB
   GL_STATIC_COPY_ARB
   GL_DYNAMIC_DRAW_ARB
   GL_DYNAMIC_READ_ARB
   GL_DYNAMIC_COPY_ARB

   GL_READ_ONLY_ARB
   GL_WRITE_ONLY_ARB
   GL_READ_WRITE_ARB

   GL_BUFFER_SIZE_ARB
   GL_BUFFER_USAGE_ARB
   GL_BUFFER_ACCESS_ARB
   GL_BUFFER_MAPPED_ARB
   GL_BUFFER_MAP_POINTER_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_vertex_buffer_object (gl:QueryExtension "GL_ARB_vertex_buffer_object"))

   (define GLintptrARB fft-signed-long) ;ptrdiff_t
   (define GLsizeiptrARB fft-unsigned-long) ;ptrdiff_t

   (setq GL gl:GetProcAddress)
   (define glBindBufferARB (GL GLvoid "glBindBufferARB" GLenum GLuint))
   (define glDeleteBuffersARB (GL GLvoid "glDeleteBuffersARB" GLsizei GLuint*))
   (define glGenBuffersARB (GL GLvoid "glGenBuffersARB" GLsizei GLuint*))
   (define glIsBufferARB (GL GLboolean "glIsBufferARB" GLuint))
   (define glBufferDataARB (GL GLvoid "glBufferDataARB" GLenum GLsizeiptrARB fft-any GLenum))
   (define glBufferSubDataARB (GL GLvoid "glBufferSubDataARB" GLenum GLintptrARB GLsizeiptrARB fft-any))
   (define glGetBufferSubDataARB (GL GLvoid "glGetBufferSubDataARB" GLenum GLintptrARB GLsizeiptrARB fft-any))
   (define glMapBufferARB (GL type-vptr "glMapBufferARB" GLenum GLenum))
   (define glUnmapBufferARB (GL GLboolean "glUnmapBufferARB" GLenum))
   (define glGetBufferParameterivARB (GL GLvoid "glGetBufferParameterivARB" GLenum GLenum GLint*))
   (define glGetBufferPointervARB (GL GLvoid "glGetBufferPointervARB" GLenum GLenum fft-any))

   (define GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH_ARB #x2095)
   (define GL_ARRAY_BUFFER_ARB                              #x8892)
   (define GL_ELEMENT_ARRAY_BUFFER_ARB                      #x8893)
   (define GL_ARRAY_BUFFER_BINDING_ARB                      #x8894)
   (define GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB              #x8895)
   (define GL_VERTEX_ARRAY_BUFFER_BINDING_ARB               #x8896)
   (define GL_NORMAL_ARRAY_BUFFER_BINDING_ARB               #x8897)
   (define GL_COLOR_ARRAY_BUFFER_BINDING_ARB                #x8898)
   (define GL_INDEX_ARRAY_BUFFER_BINDING_ARB                #x8899)
   (define GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB        #x889A)
   (define GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB            #x889B)
   (define GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB      #x889C)
   (define GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB       #x889D)
   (define GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB               #x889E)
   (define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB        #x889F)
   (define GL_STREAM_DRAW_ARB                               #x88E0)
   (define GL_STREAM_READ_ARB                               #x88E1)
   (define GL_STREAM_COPY_ARB                               #x88E2)
   (define GL_STATIC_DRAW_ARB                               #x88E4)
   (define GL_STATIC_READ_ARB                               #x88E5)
   (define GL_STATIC_COPY_ARB                               #x88E6)
   (define GL_DYNAMIC_DRAW_ARB                              #x88E8)
   (define GL_DYNAMIC_READ_ARB                              #x88E9)
   (define GL_DYNAMIC_COPY_ARB                              #x88EA)
   (define GL_READ_ONLY_ARB                                 #x88B8)
   (define GL_WRITE_ONLY_ARB                                #x88B9)
   (define GL_READ_WRITE_ARB                                #x88BA)
   (define GL_BUFFER_SIZE_ARB                               #x8764)
   (define GL_BUFFER_USAGE_ARB                              #x8765)
   (define GL_BUFFER_ACCESS_ARB                             #x88BB)
   (define GL_BUFFER_MAPPED_ARB                             #x88BC)
   (define GL_BUFFER_MAP_POINTER_ARB                        #x88BD)

))
