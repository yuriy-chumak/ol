; OpenGL 1.5 (29 Jul 2003)
;  + ARB vertex_buffer_object
;  + ARB occlusion_query
;  + EXT shadow_funcs

(define-library (OpenGL 1.5)
(export
      (exports (OpenGL 1.4))

   GL_VERSION_1_5

;; H.1 ARB_vertex_buffer_object
   GLintptr GLsizeiptr

   glBindBuffer
   glDeleteBuffers
   glGenBuffers
   glIsBuffer
   glBufferData
   glBufferSubData
   glGetBufferSubData
   glMapBuffer
   glUnmapBuffer
   glGetBufferParameteriv
   glGetBufferPointerv

   GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH
   GL_ARRAY_BUFFER
   GL_ELEMENT_ARRAY_BUFFER
   GL_ARRAY_BUFFER_BINDING
   GL_ELEMENT_ARRAY_BUFFER_BINDING
   GL_VERTEX_ARRAY_BUFFER_BINDING
   GL_NORMAL_ARRAY_BUFFER_BINDING
   GL_COLOR_ARRAY_BUFFER_BINDING
   GL_INDEX_ARRAY_BUFFER_BINDING
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING
   GL_WEIGHT_ARRAY_BUFFER_BINDING
   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
   GL_STREAM_DRAW
   GL_STREAM_READ
   GL_STREAM_COPY
   GL_STATIC_DRAW
   GL_STATIC_READ
   GL_STATIC_COPY
   GL_DYNAMIC_DRAW
   GL_DYNAMIC_READ
   GL_DYNAMIC_COPY
   GL_READ_ONLY
   GL_WRITE_ONLY
   GL_READ_WRITE
   GL_BUFFER_SIZE
   GL_BUFFER_USAGE
   GL_BUFFER_ACCESS
   GL_BUFFER_MAPPED
   GL_BUFFER_MAP_POINTER

;; H.2 ARB_occlusion_query
   GL_SAMPLES_PASSED
   GL_QUERY_COUNTER_BITS
   GL_CURRENT_QUERY
   GL_QUERY_RESULT
   GL_QUERY_RESULT_AVAILABLE

   glGenQueries
   glDeleteQueries
   glIsQuery
   glBeginQuery
   glEndQuery
   glGetQueryiv
   glGetQueryObjectiv
   glGetQueryObjectuiv

;; H.3 EXT_shadow_funcs

;; H.4 Changed Tokens
   GL_FOG_COORD_SRC
   GL_FOG_COORD
   GL_CURRENT_FOG_COORD
   GL_FOG_COORD_ARRAY_TYPE
   GL_FOG_COORD_ARRAY_STRIDE
   GL_FOG_COORD_ARRAY_POINTER
   GL_FOG_COORD_ARRAY
   GL_SRC0_RGB
   GL_SRC1_RGB
   GL_SRC2_RGB
   GL_SRC0_ALPHA
   GL_SRC1_ALPHA
   GL_SRC2_ALPHA
   GL_FOG_COORD_ARRAY_BUFFER_BINDING ; not in standard, but in .h file

)

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
        (OpenGL 1.4))

(begin
   (define GL_VERSION_1_5 1)

   (setq GL gl:GetProcAddress)

 ; ARB_vertex_buffer_object
   (define GLintptr fft-signed-long) ;ptrdiff_t
   (define GLsizeiptr fft-unsigned-long) ;ptrdiff_t

   (define glBindBuffer (GL GLvoid "glBindBuffer" GLenum GLuint))
   (define glDeleteBuffers (GL GLvoid "glDeleteBuffers" GLsizei GLuint*))
   (define glGenBuffers (GL GLvoid "glGenBuffers" GLsizei GLuint*))
   (define glIsBuffer (GL GLboolean "glIsBuffer" GLuint))
   (define glBufferData (GL GLvoid "glBufferData" GLenum GLsizeiptr fft-any GLenum))
   (define glBufferSubData (GL GLvoid "glBufferSubData" GLenum GLintptr GLsizeiptr fft-any))
   (define glGetBufferSubData (GL GLvoid "glGetBufferSubData" GLenum GLintptr GLsizeiptr fft-any))
   (define glMapBuffer (GL type-vptr "glMapBufferARB" GLenum GLenum))
   (define glUnmapBuffer (GL GLboolean "glUnmapBuffer" GLenum))
   (define glGetBufferParameteriv (GL GLvoid "glGetBufferParameteriv" GLenum GLenum GLint*))
   (define glGetBufferPointerv (GL GLvoid "glGetBufferPointerv" GLenum GLenum fft-any))

   (define GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH #x2095)
   (define GL_ARRAY_BUFFER                              #x8892)
   (define GL_ELEMENT_ARRAY_BUFFER                      #x8893)
   (define GL_ARRAY_BUFFER_BINDING                      #x8894)
   (define GL_ELEMENT_ARRAY_BUFFER_BINDING              #x8895)
   (define GL_VERTEX_ARRAY_BUFFER_BINDING               #x8896)
   (define GL_NORMAL_ARRAY_BUFFER_BINDING               #x8897)
   (define GL_COLOR_ARRAY_BUFFER_BINDING                #x8898)
   (define GL_INDEX_ARRAY_BUFFER_BINDING                #x8899)
   (define GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING        #x889A)
   (define GL_EDGE_FLAG_ARRAY_BUFFER_BINDING            #x889B)
   (define GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING      #x889C)
   (define GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING       #x889D)
   (define GL_WEIGHT_ARRAY_BUFFER_BINDING               #x889E)
   (define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING        #x889F)
   (define GL_STREAM_DRAW                               #x88E0)
   (define GL_STREAM_READ                               #x88E1)
   (define GL_STREAM_COPY                               #x88E2)
   (define GL_STATIC_DRAW                               #x88E4)
   (define GL_STATIC_READ                               #x88E5)
   (define GL_STATIC_COPY                               #x88E6)
   (define GL_DYNAMIC_DRAW                              #x88E8)
   (define GL_DYNAMIC_READ                              #x88E9)
   (define GL_DYNAMIC_COPY                              #x88EA)
   (define GL_READ_ONLY                                 #x88B8)
   (define GL_WRITE_ONLY                                #x88B9)
   (define GL_READ_WRITE                                #x88BA)
   (define GL_BUFFER_SIZE                               #x8764)
   (define GL_BUFFER_USAGE                              #x8765)
   (define GL_BUFFER_ACCESS                             #x88BB)
   (define GL_BUFFER_MAPPED                             #x88BC)
   (define GL_BUFFER_MAP_POINTER                        #x88BD)

 ; ARB_occlusion_query

   (define glGenQueries (GL GLvoid "GenQueries" GLsizei GLuint*))
   (define glDeleteQueries (GL GLvoid "DeleteQueries" GLsizei GLuint*))
   (define glIsQuery (GL GLboolean "IsQuery" GLuint))
   (define glBeginQuery (GL GLvoid "BeginQuery" GLenum GLuint))
   (define glEndQuery (GL GLvoid "EndQuery" GLenum))
   (define glGetQueryiv (GL GLvoid "GetQueryiv" GLenum GLenum GLint*))
   (define glGetQueryObjectiv (GL GLvoid "GetQueryObjectiv" GLuint GLenum GLint*))
   (define glGetQueryObjectuiv (GL GLvoid "GetQueryObjectuiv" GLuint GLenum GLuint*))

   (define GL_SAMPLES_PASSED                            #x8914)
   (define GL_QUERY_COUNTER_BITS                        #x8864)
   (define GL_CURRENT_QUERY                             #x8865)
   (define GL_QUERY_RESULT                              #x8866)
   (define GL_QUERY_RESULT_AVAILABLE                    #x8867)

 ; New token names and the old names they replace
   (define GL_FOG_COORD_SRC           GL_FOG_COORDINATE_SOURCE)
   (define GL_FOG_COORD               GL_FOG_COORDINATE)
   (define GL_CURRENT_FOG_COORD       GL_CURRENT_FOG_COORDINATE)
   (define GL_FOG_COORD_ARRAY_TYPE    GL_FOG_COORDINATE_ARRAY_TYPE)
   (define GL_FOG_COORD_ARRAY_STRIDE  GL_FOG_COORDINATE_ARRAY_STRIDE)
   (define GL_FOG_COORD_ARRAY_POINTER GL_FOG_COORDINATE_ARRAY_POINTER)
   (define GL_FOG_COORD_ARRAY         GL_FOG_COORDINATE_ARRAY)
   (define GL_SRC0_RGB                GL_SOURCE0_RGB)
   (define GL_SRC1_RGB                GL_SOURCE1_RGB)
   (define GL_SRC2_RGB                GL_SOURCE2_RGB)
   (define GL_SRC0_ALPHA              GL_SOURCE0_ALPHA)
   (define GL_SRC1_ALPHA              GL_SOURCE1_ALPHA)
   (define GL_SRC2_ALPHA              GL_SOURCE2_ALPHA)
   ; not in standard, but in .h file:
   (define GL_FOG_COORD_ARRAY_BUFFER_BINDING GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING)

))
