; OpenGL 1.5 (2003)

(define-library (OpenGL version-1-5)
(export

   GL_VERSION_1_5

 ; ARB_vertex_buffer_object
   GLintptr GLsizeiptr

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

   glBindBuffer ; void (GLenum, GLuint)
   ;; GLAPI void APIENTRY glDeleteBuffers (GLsizei, const GLuint *);
   glGenBuffers ; void (GLsizei, GLuint *)
   ;; GLAPI GLboolean APIENTRY glIsBuffer (GLuint);
   glBufferData ; void (GLenum, GLsizeiptr, const GLvoid *, GLenum)
   ;; GLAPI void APIENTRY glBufferSubData (GLenum, GLintptr, GLsizeiptr, const GLvoid *);
   ;; GLAPI void APIENTRY glGetBufferSubData (GLenum, GLintptr, GLsizeiptr, GLvoid *);
   ;; GLAPI GLvoid* APIENTRY glMapBuffer (GLenum, GLenum);
   ;; GLAPI GLboolean APIENTRY glUnmapBuffer (GLenum);
   ;; GLAPI void APIENTRY glGetBufferParameteriv (GLenum, GLenum, GLint *);
   ;; GLAPI void APIENTRY glGetBufferPointerv (GLenum, GLenum, GLvoid* *);
   ;; glXCreateContextAttribs, GLX_CONTEXT_ALLOW_BUFFER_BYTE_ORDER_MISMATCH_ARB

 ; ...
   ;; GLAPI void APIENTRY glGenQueries (GLsizei, GLuint *);
   ;; GLAPI void APIENTRY glDeleteQueries (GLsizei, const GLuint *);
   ;; GLAPI GLboolean APIENTRY glIsQuery (GLuint);
   ;; GLAPI void APIENTRY glBeginQuery (GLenum, GLuint);
   ;; GLAPI void APIENTRY glEndQuery (GLenum);
   ;; GLAPI void APIENTRY glGetQueryiv (GLenum, GLenum, GLint *);
   ;; GLAPI void APIENTRY glGetQueryObjectiv (GLuint, GLenum, GLint *);
   ;; GLAPI void APIENTRY glGetQueryObjectuiv (GLuint, GLenum, GLuint *);


   (exports (OpenGL version-1-4)))

(import (scheme core)
   (OpenGL version-1-4))

(begin
   (define GL_VERSION_1_5 1)

   (define GL GL_LIBRARY)

 ; ARB_vertex_buffer_object
   (define GLintptr fft-signed-long) ;ptrdiff_t
   (define GLsizeiptr fft-unsigned-long) ;ptrdiff_t

   (define GL_ARRAY_BUFFER                   #x8892)
   (define GL_ELEMENT_ARRAY_BUFFER           #x8893)

   (define GL_ARRAY_BUFFER_BINDING           #x8894)
   (define GL_ELEMENT_ARRAY_BUFFER_BINDING   #x8895)
   (define GL_VERTEX_ARRAY_BUFFER_BINDING    #x8896)
   (define GL_NORMAL_ARRAY_BUFFER_BINDING    #x8897)
   (define GL_COLOR_ARRAY_BUFFER_BINDING     #x8898)
   (define GL_INDEX_ARRAY_BUFFER_BINDING     #x8899)
   (define GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING #x889A)
   (define GL_EDGE_FLAG_ARRAY_BUFFER_BINDING #x889B)
   (define GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING #x889C)
   (define GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING #x889D)
   (define GL_WEIGHT_ARRAY_BUFFER_BINDING    #x889E)

   (define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING #x889F)

   (define GL_STREAM_DRAW                    #x88E0)
   (define GL_STREAM_READ                    #x88E1)
   (define GL_STREAM_COPY                    #x88E2)
   (define GL_STATIC_DRAW                    #x88E4)
   (define GL_STATIC_READ                    #x88E5)
   (define GL_STATIC_COPY                    #x88E6)
   (define GL_DYNAMIC_DRAW                   #x88E8)
   (define GL_DYNAMIC_READ                   #x88E9)
   (define GL_DYNAMIC_COPY                   #x88EA)

   (define GL_READ_ONLY                      #x88B8)
   (define GL_WRITE_ONLY                     #x88B9)
   (define GL_READ_WRITE                     #x88BA)

   (define GL_BUFFER_SIZE                    #x8764)
   (define GL_BUFFER_USAGE                   #x8765)
   (define GL_BUFFER_ACCESS                  #x88BB)
   (define GL_BUFFER_MAPPED                  #x88BC)

   (define GL_BUFFER_MAP_POINTER             #x88BD)

   (define glBindBuffer (GL GLvoid "glBindBuffer" GLenum GLuint))
   (define glDeleteBuffers (GL GLvoid "glDeleteBuffers" GLsizei GLuint*))
   (define glGenBuffers (GL GLvoid "glGenBuffers" GLsizei GLuint&))
;; GLAPI GLboolean APIENTRY glIsBuffer (GLuint);
   (define glBufferData (GL GLvoid "glBufferData" GLenum GLsizeiptr fft-any GLenum))
;; GLAPI void APIENTRY glBufferSubData (GLenum, GLintptr, GLsizeiptr, const GLvoid *);
;; GLAPI void APIENTRY glGetBufferSubData (GLenum, GLintptr, GLsizeiptr, GLvoid *);
;; GLAPI GLvoid* APIENTRY glMapBuffer (GLenum, GLenum);
;; GLAPI GLboolean APIENTRY glUnmapBuffer (GLenum);
;; GLAPI void APIENTRY glGetBufferParameteriv (GLenum, GLenum, GLint *);
;; GLAPI void APIENTRY glGetBufferPointerv (GLenum, GLenum, GLvoid* *);

 ; ARB_occlusion_query

 ; EXT_shadow_funcs

 ; ...
;; #define GL_QUERY_COUNTER_BITS             #x8864
;; #define GL_CURRENT_QUERY                  #x8865
;; #define GL_QUERY_RESULT                   #x8866
;; #define GL_QUERY_RESULT_AVAILABLE         #x8867
;; #define GL_SAMPLES_PASSED                 #x8914
;; #define GL_FOG_COORD_SRC                  GL_FOG_COORDINATE_SOURCE
;; #define GL_FOG_COORD                      GL_FOG_COORDINATE
;; #define GL_CURRENT_FOG_COORD              GL_CURRENT_FOG_COORDINATE
;; #define GL_FOG_COORD_ARRAY_TYPE           GL_FOG_COORDINATE_ARRAY_TYPE
;; #define GL_FOG_COORD_ARRAY_STRIDE         GL_FOG_COORDINATE_ARRAY_STRIDE
;; #define GL_FOG_COORD_ARRAY_POINTER        GL_FOG_COORDINATE_ARRAY_POINTER
;; #define GL_FOG_COORD_ARRAY                GL_FOG_COORDINATE_ARRAY
;; #define GL_FOG_COORD_ARRAY_BUFFER_BINDING GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING
;; #define GL_SRC0_RGB                       GL_SOURCE0_RGB
;; #define GL_SRC1_RGB                       GL_SOURCE1_RGB
;; #define GL_SRC2_RGB                       GL_SOURCE2_RGB
;; #define GL_SRC0_ALPHA                     GL_SOURCE0_ALPHA
;; #define GL_SRC1_ALPHA                     GL_SOURCE1_ALPHA
;; #define GL_SRC2_ALPHA                     GL_SOURCE2_ALPHA

;; GLAPI void APIENTRY glGenQueries (GLsizei, GLuint *);
;; GLAPI void APIENTRY glDeleteQueries (GLsizei, const GLuint *);
;; GLAPI GLboolean APIENTRY glIsQuery (GLuint);
;; GLAPI void APIENTRY glBeginQuery (GLenum, GLuint);
;; GLAPI void APIENTRY glEndQuery (GLenum);
;; GLAPI void APIENTRY glGetQueryiv (GLenum, GLenum, GLint *);
;; GLAPI void APIENTRY glGetQueryObjectiv (GLuint, GLenum, GLint *);
;; GLAPI void APIENTRY glGetQueryObjectuiv (GLuint, GLenum, GLuint *);


))