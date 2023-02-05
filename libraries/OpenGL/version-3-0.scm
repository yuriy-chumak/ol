; OpenGL 3.0 (11 Aug 2008) GLSL 1.3
(define-library (OpenGL version-3-0)
(export
; this version provides new functionality over old
; so, OpenGL 1.0, 1.1, ..., 2.1 still available.
; disabling of old functionality was introduced in 3.2
   gl:CreateContextAttribs ; os independent context creation function

   GL_VERSION_3_0

   GL_COMPARE_REF_TO_TEXTURE
   GL_CLIP_DISTANCE0
   GL_CLIP_DISTANCE1
   GL_CLIP_DISTANCE2
   GL_CLIP_DISTANCE3
   GL_CLIP_DISTANCE4
   GL_CLIP_DISTANCE5
   GL_MAX_CLIP_DISTANCES
   GL_MAJOR_VERSION
   GL_MINOR_VERSION
   GL_NUM_EXTENSIONS
   GL_CONTEXT_FLAGS
   GL_DEPTH_BUFFER
   GL_STENCIL_BUFFER
   GL_COMPRESSED_RED
   GL_COMPRESSED_RG
   GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT
   GL_RGBA32F
   GL_RGB32F
   GL_RGBA16F
   GL_RGB16F
   GL_VERTEX_ATTRIB_ARRAY_INTEGER
   GL_MAX_ARRAY_TEXTURE_LAYERS
   GL_MIN_PROGRAM_TEXEL_OFFSET
   GL_MAX_PROGRAM_TEXEL_OFFSET
   GL_CLAMP_VERTEX_COLOR
   GL_CLAMP_FRAGMENT_COLOR
   GL_CLAMP_READ_COLOR
   GL_FIXED_ONLY
   GL_MAX_VARYING_COMPONENTS
   GL_TEXTURE_RED_TYPE
   GL_TEXTURE_GREEN_TYPE
   GL_TEXTURE_BLUE_TYPE
   GL_TEXTURE_ALPHA_TYPE
   GL_TEXTURE_LUMINANCE_TYPE
   GL_TEXTURE_INTENSITY_TYPE
   GL_TEXTURE_DEPTH_TYPE
   GL_UNSIGNED_NORMALIZED
   GL_TEXTURE_1D_ARRAY
   GL_PROXY_TEXTURE_1D_ARRAY
   GL_TEXTURE_2D_ARRAY
   GL_PROXY_TEXTURE_2D_ARRAY
   GL_TEXTURE_BINDING_1D_ARRAY
   GL_TEXTURE_BINDING_2D_ARRAY
   GL_R11F_G11F_B10F
   GL_UNSIGNED_INT_10F_11F_11F_REV
   GL_RGB9_E5
   GL_UNSIGNED_INT_5_9_9_9_REV
   GL_TEXTURE_SHARED_SIZE
   GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH
   GL_TRANSFORM_FEEDBACK_BUFFER_MODE
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS
   GL_TRANSFORM_FEEDBACK_VARYINGS
   GL_TRANSFORM_FEEDBACK_BUFFER_START
   GL_TRANSFORM_FEEDBACK_BUFFER_SIZE
   GL_PRIMITIVES_GENERATED
   GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN
   GL_RASTERIZER_DISCARD
   GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS
   GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS
   GL_INTERLEAVED_ATTRIBS
   GL_SEPARATE_ATTRIBS
   GL_TRANSFORM_FEEDBACK_BUFFER
   GL_TRANSFORM_FEEDBACK_BUFFER_BINDING
   GL_RGBA32UI
   GL_RGB32UI
   GL_RGBA16UI
   GL_RGB16UI
   GL_RGBA8UI
   GL_RGB8UI
   GL_RGBA32I
   GL_RGB32I
   GL_RGBA16I
   GL_RGB16I
   GL_RGBA8I
   GL_RGB8I
   GL_RED_INTEGER
   GL_GREEN_INTEGER
   GL_BLUE_INTEGER
   GL_ALPHA_INTEGER
   GL_RGB_INTEGER
   GL_RGBA_INTEGER
   GL_BGR_INTEGER
   GL_BGRA_INTEGER
   GL_SAMPLER_1D_ARRAY
   GL_SAMPLER_2D_ARRAY
   GL_SAMPLER_1D_ARRAY_SHADOW
   GL_SAMPLER_2D_ARRAY_SHADOW
   GL_SAMPLER_CUBE_SHADOW
   GL_UNSIGNED_INT_VEC2
   GL_UNSIGNED_INT_VEC3
   GL_UNSIGNED_INT_VEC4
   GL_INT_SAMPLER_1D
   GL_INT_SAMPLER_2D
   GL_INT_SAMPLER_3D
   GL_INT_SAMPLER_CUBE
   GL_INT_SAMPLER_1D_ARRAY
   GL_INT_SAMPLER_2D_ARRAY
   GL_UNSIGNED_INT_SAMPLER_1D
   GL_UNSIGNED_INT_SAMPLER_2D
   GL_UNSIGNED_INT_SAMPLER_3D
   GL_UNSIGNED_INT_SAMPLER_CUBE
   GL_UNSIGNED_INT_SAMPLER_1D_ARRAY
   GL_UNSIGNED_INT_SAMPLER_2D_ARRAY
   GL_QUERY_WAIT
   GL_QUERY_NO_WAIT
   GL_QUERY_BY_REGION_WAIT
   GL_QUERY_BY_REGION_NO_WAIT
;; /* Reuse tokens from ARB_depth_buffer_float */
;; /* reuse GL_DEPTH_COMPONENT32F */
;; /* reuse GL_DEPTH32F_STENCIL8 */
;; /* reuse GL_FLOAT_32_UNSIGNED_INT_24_8_REV */
;; /* Reuse tokens from ARB_framebuffer_sRGB */
;; /* reuse GL_FRAMEBUFFER_SRGB */
;; /* Reuse tokens from ARB_half_float_vertex */
;; /* reuse GL_HALF_FLOAT */
;; /* Reuse tokens from ARB_map_buffer_range */
;; /* reuse GL_MAP_READ_BIT */
;; /* reuse GL_MAP_WRITE_BIT */
;; /* reuse GL_MAP_INVALIDATE_RANGE_BIT */
;; /* reuse GL_MAP_INVALIDATE_BUFFER_BIT */
;; /* reuse GL_MAP_FLUSH_EXPLICIT_BIT */
;; /* reuse GL_MAP_UNSYNCHRONIZED_BIT */
;; /* Reuse tokens from ARB_texture_compression_rgtc */
;; /* reuse GL_COMPRESSED_RED_RGTC1 */
;; /* reuse GL_COMPRESSED_SIGNED_RED_RGTC1 */
;; /* reuse GL_COMPRESSED_RG_RGTC2 */
;; /* reuse GL_COMPRESSED_SIGNED_RG_RGTC2 */
;; /* Reuse tokens from ARB_texture_rg */
;; /* reuse GL_RG */
;; /* reuse GL_RG_INTEGER */
;; /* reuse GL_R8 */
;; /* reuse GL_R16 */
;; /* reuse GL_RG8 */
;; /* reuse GL_RG16 */
;; /* reuse GL_R16F */
;; /* reuse GL_R32F */
;; /* reuse GL_RG16F */
;; /* reuse GL_RG32F */
;; /* reuse GL_R8I */
;; /* reuse GL_R8UI */
;; /* reuse GL_R16I */
;; /* reuse GL_R16UI */
;; /* reuse GL_R32I */
;; /* reuse GL_R32UI */
;; /* reuse GL_RG8I */
;; /* reuse GL_RG8UI */
;; /* reuse GL_RG16I */
;; /* reuse GL_RG16UI */
;; /* reuse GL_RG32I */
;; /* reuse GL_RG32UI */

   glColorMaski; GLAPI void APIENTRY  (GLuint, GLboolean, GLboolean, GLboolean, GLboolean)
   glGetBooleani_v; GLAPI void APIENTRY  (GLenum, GLuint, GLboolean *)
   glGetIntegeri_v; GLAPI void APIENTRY  (GLenum, GLuint, GLint *)
   glEnablei; GLAPI void APIENTRY  (GLenum, GLuint)
   glDisablei; GLAPI void APIENTRY  (GLenum, GLuint)
   glIsEnabledi; GLAPI GLboolean APIENTRY  (GLenum, GLuint)
   glBeginTransformFeedback; GLAPI void APIENTRY  (GLenum)
   glEndTransformFeedback; GLAPI void APIENTRY  (void)
   glBindBufferRange; GLAPI void APIENTRY  (GLenum, GLuint, GLuint, GLintptr, GLsizeiptr)
   glBindBufferBase; GLAPI void APIENTRY  (GLenum, GLuint, GLuint)
   glTransformFeedbackVaryings; GLAPI void APIENTRY  (GLuint, GLsizei, const GLchar* *, GLenum)
   glGetTransformFeedbackVarying; GLAPI void APIENTRY  (GLuint, GLuint, GLsizei, GLsizei *, GLsizei *, GLenum *, GLchar *)
   glClampColor; GLAPI void APIENTRY  (GLenum, GLenum)
   glBeginConditionalRender; GLAPI void APIENTRY  (GLuint, GLenum)
   glEndConditionalRender; GLAPI void APIENTRY  (void)
   glVertexAttribI1i; GLAPI void APIENTRY  (GLuint, GLint)
   glVertexAttribI2i; GLAPI void APIENTRY  (GLuint, GLint, GLint)
   glVertexAttribI3i; GLAPI void APIENTRY  (GLuint, GLint, GLint, GLint)
   glVertexAttribI4i; GLAPI void APIENTRY  (GLuint, GLint, GLint, GLint, GLint)
   glVertexAttribI1ui; GLAPI void APIENTRY  (GLuint, GLuint)
   glVertexAttribI2ui; GLAPI void APIENTRY  (GLuint, GLuint, GLuint)
   glVertexAttribI3ui; GLAPI void APIENTRY  (GLuint, GLuint, GLuint, GLuint)
   glVertexAttribI4ui; GLAPI void APIENTRY  (GLuint, GLuint, GLuint, GLuint, GLuint)
   glVertexAttribI1iv; GLAPI void APIENTRY  (GLuint, const GLint *)
   glVertexAttribI2iv; GLAPI void APIENTRY  (GLuint, const GLint *)
   glVertexAttribI3iv; GLAPI void APIENTRY  (GLuint, const GLint *)
   glVertexAttribI4iv; GLAPI void APIENTRY  (GLuint, const GLint *)
   glVertexAttribI1uiv; GLAPI void APIENTRY  (GLuint, const GLuint *)
   glVertexAttribI2uiv; GLAPI void APIENTRY  (GLuint, const GLuint *)
   glVertexAttribI3uiv; GLAPI void APIENTRY  (GLuint, const GLuint *)
   glVertexAttribI4uiv; GLAPI void APIENTRY  (GLuint, const GLuint *)
   glVertexAttribI4bv; GLAPI void APIENTRY  (GLuint, const GLbyte *)
   glVertexAttribI4sv; GLAPI void APIENTRY  (GLuint, const GLshort *)
   glVertexAttribI4ubv; GLAPI void APIENTRY  (GLuint, const GLubyte *)
   glVertexAttribI4usv; GLAPI void APIENTRY  (GLuint, const GLushort *)
   glVertexAttribIPointer; GLAPI void APIENTRY  (GLuint, GLint, GLenum, GLsizei, const GLvoid *)
   glGetVertexAttribIiv; GLAPI void APIENTRY  (GLuint, GLenum, GLint *)
   glGetVertexAttribIuiv; GLAPI void APIENTRY  (GLuint, GLenum, GLuint *)
   glGetUniformuiv; GLAPI void APIENTRY  (GLuint, GLint, GLuint *)
   glBindFragDataLocation; GLAPI void APIENTRY  (GLuint, GLuint, const GLchar *)
   glGetFragDataLocation; GLAPI GLint APIENTRY  (GLuint, const GLchar *)
   glUniform1ui; GLAPI void APIENTRY  (GLint, GLuint)
   glUniform2ui; GLAPI void APIENTRY  (GLint, GLuint, GLuint)
   glUniform3ui; GLAPI void APIENTRY  (GLint, GLuint, GLuint, GLuint)
   glUniform4ui; GLAPI void APIENTRY  (GLint, GLuint, GLuint, GLuint, GLuint)
   glUniform1uiv; GLAPI void APIENTRY  (GLint, GLsizei, const GLuint *)
   glUniform2uiv; GLAPI void APIENTRY  (GLint, GLsizei, const GLuint *)
   glUniform3uiv; GLAPI void APIENTRY  (GLint, GLsizei, const GLuint *)
   glUniform4uiv; GLAPI void APIENTRY  (GLint, GLsizei, const GLuint *)
   glTexParameterIiv; GLAPI void APIENTRY  (GLenum, GLenum, const GLint *)
   glTexParameterIuiv; GLAPI void APIENTRY  (GLenum, GLenum, const GLuint *)
   glGetTexParameterIiv; GLAPI void APIENTRY  (GLenum, GLenum, GLint *)
   glGetTexParameterIuiv; GLAPI void APIENTRY  (GLenum, GLenum, GLuint *)
   glClearBufferiv; GLAPI void APIENTRY  (GLenum, GLint, const GLint *)
   glClearBufferuiv; GLAPI void APIENTRY  (GLenum, GLint, const GLuint *)
   glClearBufferfv; GLAPI void APIENTRY  (GLenum, GLint, const GLfloat *)
   glClearBufferfi; GLAPI void APIENTRY  (GLenum, GLint, GLfloat, GLint)
   glGetStringi; GLAPI const GLubyte * APIENTRY (GLenum, GLuint)

 ; ARB_vertex_array_object
   GL_ARB_vertex_array_object
   GL_VERTEX_ARRAY_BINDING

   glBindVertexArray; GLAPI void APIENTRY (GLuint)
   glDeleteVertexArrays; GLAPI void APIENTRY (GLsizei, const GLuint *)
   glGenVertexArrays; GLAPI void APIENTRY (GLsizei, GLuint *)
   glIsVertexArray; GLAPI GLboolean APIENTRY (GLuint)
   
 ; ARB_framebuffer_object
   GL_ARB_framebuffer_object
   GL_INVALID_FRAMEBUFFER_OPERATION
   GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING
   GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE
   GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE
   GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE
   GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE
   GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE
   GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE
   GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE
   GL_FRAMEBUFFER_DEFAULT
   GL_FRAMEBUFFER_UNDEFINED
   GL_DEPTH_STENCIL_ATTACHMENT
   GL_INDEX
   GL_MAX_RENDERBUFFER_SIZE
   GL_DEPTH_STENCIL
   GL_UNSIGNED_INT_24_8
   GL_DEPTH24_STENCIL8
   GL_TEXTURE_STENCIL_SIZE
   GL_FRAMEBUFFER_BINDING
   GL_DRAW_FRAMEBUFFER_BINDING
   GL_RENDERBUFFER_BINDING
   GL_READ_FRAMEBUFFER
   GL_DRAW_FRAMEBUFFER
   GL_READ_FRAMEBUFFER_BINDING
   GL_RENDERBUFFER_SAMPLES
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER
   GL_FRAMEBUFFER_COMPLETE
   GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
   GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
   GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER
   GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER
   GL_FRAMEBUFFER_UNSUPPORTED
   GL_MAX_COLOR_ATTACHMENTS
   GL_COLOR_ATTACHMENT0
   GL_COLOR_ATTACHMENT1
   GL_COLOR_ATTACHMENT2
   GL_COLOR_ATTACHMENT3
   GL_COLOR_ATTACHMENT4
   GL_COLOR_ATTACHMENT5
   GL_COLOR_ATTACHMENT6
   GL_COLOR_ATTACHMENT7
   GL_COLOR_ATTACHMENT8
   GL_COLOR_ATTACHMENT9
   GL_COLOR_ATTACHMENT10
   GL_COLOR_ATTACHMENT11
   GL_COLOR_ATTACHMENT12
   GL_COLOR_ATTACHMENT13
   GL_COLOR_ATTACHMENT14
   GL_COLOR_ATTACHMENT15
   GL_DEPTH_ATTACHMENT
   GL_STENCIL_ATTACHMENT
   GL_FRAMEBUFFER
   GL_RENDERBUFFER
   GL_RENDERBUFFER_WIDTH
   GL_RENDERBUFFER_HEIGHT
   GL_RENDERBUFFER_INTERNAL_FORMAT
   GL_STENCIL_INDEX1
   GL_STENCIL_INDEX4
   GL_STENCIL_INDEX8
   GL_STENCIL_INDEX16
   GL_RENDERBUFFER_RED_SIZE
   GL_RENDERBUFFER_GREEN_SIZE
   GL_RENDERBUFFER_BLUE_SIZE
   GL_RENDERBUFFER_ALPHA_SIZE
   GL_RENDERBUFFER_DEPTH_SIZE
   GL_RENDERBUFFER_STENCIL_SIZE
   GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE
   GL_MAX_SAMPLES
      
   glIsRenderbuffer; GLAPI GLboolean (GLuint)
   glBindRenderbuffer; GLAPI void (GLenum, GLuint)
   glDeleteRenderbuffers; GLAPI void (GLsizei, const GLuint *)
   glGenRenderbuffers; GLAPI void (GLsizei, GLuint *)
   glRenderbufferStorage; GLAPI void (GLenum, GLenum, GLsizei, GLsizei)
   glGetRenderbufferParameteriv; GLAPI void (GLenum, GLenum, GLint *)
   glIsFramebuffer; GLAPI GLboolean (GLuint)
   glBindFramebuffer; GLAPI void (GLenum, GLuint)
   glDeleteFramebuffers; GLAPI void (GLsizei, const GLuint *)
   glGenFramebuffers; GLAPI void (GLsizei, GLuint *)
   glCheckFramebufferStatus; GLAPI GLenum (GLenum)
   glFramebufferTexture1D; GLAPI void (GLenum, GLenum, GLenum, GLuint, GLint)
   glFramebufferTexture2D; GLAPI void (GLenum, GLenum, GLenum, GLuint, GLint)
   glFramebufferTexture3D; GLAPI void (GLenum, GLenum, GLenum, GLuint, GLint, GLint)
   glFramebufferRenderbuffer; GLAPI void (GLenum, GLenum, GLenum, GLuint)
   glGetFramebufferAttachmentParameteriv; GLAPI void (GLenum, GLenum, GLenum, GLint *)
   glGenerateMipmap; GLAPI void (GLenum)
   glBlitFramebuffer; GLAPI void (GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLint, GLbitfield, GLenum)
   glRenderbufferStorageMultisample; GLAPI void (GLenum, GLsizei, GLenum, GLsizei, GLsizei)
   glFramebufferTextureLayer; GLAPI void (GLenum, GLenum, GLuint, GLint, GLint)

   (exports (OpenGL version-2-1)))

(import (scheme core)
   (OpenGL version-2-1))

; os independent context creation function:
(cond-expand
   (Windows
      (import (OpenGL WGL ARB create_context))
      (begin
         (define gl:CreateContextAttribs wglCreateContextAttribsARB)))
   (else
      (import (OpenGL GLX ARB create_context))
      (begin
         (define gl:CreateContextAttribs glXCreateContextAttribsARB))))

(begin
   (define GL_VERSION_3_0 1)

   (define GL_COMPARE_REF_TO_TEXTURE         #x884E) ; GL_COMPARE_R_TO_TEXTURE_ARB (GL_ARB_shadow)
   (define GL_CLIP_DISTANCE0                 GL_CLIP_PLANE0)
   (define GL_CLIP_DISTANCE1                 GL_CLIP_PLANE1)
   (define GL_CLIP_DISTANCE2                 GL_CLIP_PLANE2)
   (define GL_CLIP_DISTANCE3                 GL_CLIP_PLANE3)
   (define GL_CLIP_DISTANCE4                 GL_CLIP_PLANE4)
   (define GL_CLIP_DISTANCE5                 GL_CLIP_PLANE5)
   (define GL_MAX_CLIP_DISTANCES             GL_MAX_CLIP_PLANES)
   (define GL_MAJOR_VERSION                  #x821B)
   (define GL_MINOR_VERSION                  #x821C)
   (define GL_NUM_EXTENSIONS                 #x821D)
   (define GL_CONTEXT_FLAGS                  #x821E)
   (define GL_DEPTH_BUFFER                   #x8223)
   (define GL_STENCIL_BUFFER                 #x8224)
   (define GL_COMPRESSED_RED                 #x8225)
   (define GL_COMPRESSED_RG                  #x8226)
   (define GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT #x0001)
   (define GL_RGBA32F                        #x8814)
   (define GL_RGB32F                         #x8815)
   (define GL_RGBA16F                        #x881A)
   (define GL_RGB16F                         #x881B)
   (define GL_VERTEX_ATTRIB_ARRAY_INTEGER    #x88FD)
   (define GL_MAX_ARRAY_TEXTURE_LAYERS       #x88FF)
   (define GL_MIN_PROGRAM_TEXEL_OFFSET       #x8904)
   (define GL_MAX_PROGRAM_TEXEL_OFFSET       #x8905)
   (define GL_CLAMP_VERTEX_COLOR             #x891A)
   (define GL_CLAMP_FRAGMENT_COLOR           #x891B)
   (define GL_CLAMP_READ_COLOR               #x891C)
   (define GL_FIXED_ONLY                     #x891D)
   (define GL_MAX_VARYING_COMPONENTS         #x8B4B) ; GL_MAX_VARYING_FLOATS
   (define GL_TEXTURE_RED_TYPE               #x8C10)
   (define GL_TEXTURE_GREEN_TYPE             #x8C11)
   (define GL_TEXTURE_BLUE_TYPE              #x8C12)
   (define GL_TEXTURE_ALPHA_TYPE             #x8C13)
   (define GL_TEXTURE_LUMINANCE_TYPE         #x8C14)
   (define GL_TEXTURE_INTENSITY_TYPE         #x8C15)
   (define GL_TEXTURE_DEPTH_TYPE             #x8C16)
   (define GL_UNSIGNED_NORMALIZED            #x8C17)
   (define GL_TEXTURE_1D_ARRAY               #x8C18)
   (define GL_PROXY_TEXTURE_1D_ARRAY         #x8C19)
   (define GL_TEXTURE_2D_ARRAY               #x8C1A)
   (define GL_PROXY_TEXTURE_2D_ARRAY         #x8C1B)
   (define GL_TEXTURE_BINDING_1D_ARRAY       #x8C1C)
   (define GL_TEXTURE_BINDING_2D_ARRAY       #x8C1D)
   (define GL_R11F_G11F_B10F                 #x8C3A)
   (define GL_UNSIGNED_INT_10F_11F_11F_REV   #x8C3B)
   (define GL_RGB9_E5                        #x8C3D)
   (define GL_UNSIGNED_INT_5_9_9_9_REV       #x8C3E)
   (define GL_TEXTURE_SHARED_SIZE            #x8C3F)
   (define GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH #x8C76)
   (define GL_TRANSFORM_FEEDBACK_BUFFER_MODE #x8C7F)
   (define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS #x8C80)
   (define GL_TRANSFORM_FEEDBACK_VARYINGS    #x8C83)
   (define GL_TRANSFORM_FEEDBACK_BUFFER_START #x8C84)
   (define GL_TRANSFORM_FEEDBACK_BUFFER_SIZE #x8C85)
   (define GL_PRIMITIVES_GENERATED           #x8C87)
   (define GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN #x8C88)
   (define GL_RASTERIZER_DISCARD             #x8C89)
   (define GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS #x8C8A)
   (define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS #x8C8B)
   (define GL_INTERLEAVED_ATTRIBS            #x8C8C)
   (define GL_SEPARATE_ATTRIBS               #x8C8D)
   (define GL_TRANSFORM_FEEDBACK_BUFFER      #x8C8E)
   (define GL_TRANSFORM_FEEDBACK_BUFFER_BINDING #x8C8F)
   (define GL_RGBA32UI                       #x8D70)
   (define GL_RGB32UI                        #x8D71)
   (define GL_RGBA16UI                       #x8D76)
   (define GL_RGB16UI                        #x8D77)
   (define GL_RGBA8UI                        #x8D7C)
   (define GL_RGB8UI                         #x8D7D)
   (define GL_RGBA32I                        #x8D82)
   (define GL_RGB32I                         #x8D83)
   (define GL_RGBA16I                        #x8D88)
   (define GL_RGB16I                         #x8D89)
   (define GL_RGBA8I                         #x8D8E)
   (define GL_RGB8I                          #x8D8F)
   (define GL_RED_INTEGER                    #x8D94)
   (define GL_GREEN_INTEGER                  #x8D95)
   (define GL_BLUE_INTEGER                   #x8D96)
   (define GL_ALPHA_INTEGER                  #x8D97)
   (define GL_RGB_INTEGER                    #x8D98)
   (define GL_RGBA_INTEGER                   #x8D99)
   (define GL_BGR_INTEGER                    #x8D9A)
   (define GL_BGRA_INTEGER                   #x8D9B)
   (define GL_SAMPLER_1D_ARRAY               #x8DC0)
   (define GL_SAMPLER_2D_ARRAY               #x8DC1)
   (define GL_SAMPLER_1D_ARRAY_SHADOW        #x8DC3)
   (define GL_SAMPLER_2D_ARRAY_SHADOW        #x8DC4)
   (define GL_SAMPLER_CUBE_SHADOW            #x8DC5)
   (define GL_UNSIGNED_INT_VEC2              #x8DC6)
   (define GL_UNSIGNED_INT_VEC3              #x8DC7)
   (define GL_UNSIGNED_INT_VEC4              #x8DC8)
   (define GL_INT_SAMPLER_1D                 #x8DC9)
   (define GL_INT_SAMPLER_2D                 #x8DCA)
   (define GL_INT_SAMPLER_3D                 #x8DCB)
   (define GL_INT_SAMPLER_CUBE               #x8DCC)
   (define GL_INT_SAMPLER_1D_ARRAY           #x8DCE)
   (define GL_INT_SAMPLER_2D_ARRAY           #x8DCF)
   (define GL_UNSIGNED_INT_SAMPLER_1D        #x8DD1)
   (define GL_UNSIGNED_INT_SAMPLER_2D        #x8DD2)
   (define GL_UNSIGNED_INT_SAMPLER_3D        #x8DD3)
   (define GL_UNSIGNED_INT_SAMPLER_CUBE      #x8DD4)
   (define GL_UNSIGNED_INT_SAMPLER_1D_ARRAY  #x8DD6)
   (define GL_UNSIGNED_INT_SAMPLER_2D_ARRAY  #x8DD7)
   (define GL_QUERY_WAIT                     #x8E13)
   (define GL_QUERY_NO_WAIT                  #x8E14)
   (define GL_QUERY_BY_REGION_WAIT           #x8E15)
   (define GL_QUERY_BY_REGION_NO_WAIT        #x8E16)
;; /* Reuse tokens from ARB_depth_buffer_float */
;; /* reuse GL_DEPTH_COMPONENT32F */
;; /* reuse GL_DEPTH32F_STENCIL8 */
;; /* reuse GL_FLOAT_32_UNSIGNED_INT_24_8_REV */
;; /* Reuse tokens from ARB_framebuffer_sRGB */
;; /* reuse GL_FRAMEBUFFER_SRGB */
;; /* Reuse tokens from ARB_half_float_vertex */
;; /* reuse GL_HALF_FLOAT */
;; /* Reuse tokens from ARB_map_buffer_range */
;; /* reuse GL_MAP_READ_BIT */
;; /* reuse GL_MAP_WRITE_BIT */
;; /* reuse GL_MAP_INVALIDATE_RANGE_BIT */
;; /* reuse GL_MAP_INVALIDATE_BUFFER_BIT */
;; /* reuse GL_MAP_FLUSH_EXPLICIT_BIT */
;; /* reuse GL_MAP_UNSYNCHRONIZED_BIT */
;; /* Reuse tokens from ARB_texture_compression_rgtc */
;; /* reuse GL_COMPRESSED_RED_RGTC1 */
;; /* reuse GL_COMPRESSED_SIGNED_RED_RGTC1 */
;; /* reuse GL_COMPRESSED_RG_RGTC2 */
;; /* reuse GL_COMPRESSED_SIGNED_RG_RGTC2 */
;; /* Reuse tokens from ARB_texture_rg */
;; /* reuse GL_RG */
;; /* reuse GL_RG_INTEGER */
;; /* reuse GL_R8 */
;; /* reuse GL_R16 */
;; /* reuse GL_RG8 */
;; /* reuse GL_RG16 */
;; /* reuse GL_R16F */
;; /* reuse GL_R32F */
;; /* reuse GL_RG16F */
;; /* reuse GL_RG32F */
;; /* reuse GL_R8I */
;; /* reuse GL_R8UI */
;; /* reuse GL_R16I */
;; /* reuse GL_R16UI */
;; /* reuse GL_R32I */
;; /* reuse GL_R32UI */
;; /* reuse GL_RG8I */
;; /* reuse GL_RG8UI */
;; /* reuse GL_RG16I */
;; /* reuse GL_RG16UI */
;; /* reuse GL_RG32I */
;; /* reuse GL_RG32UI */

   (define GLbyte* (fft* GLbyte))
   (define GLushort* (fft* GLushort))
   (define GLchar* type-string)

   (define glColorMaski (gl:GetProcAddress GLvoid "glColorMaski" GLuint GLboolean GLboolean GLboolean GLboolean))
   (define glGetBooleani_v (gl:GetProcAddress GLvoid "glGetBooleani_v" GLenum GLuint GLboolean*))
   (define glGetIntegeri_v (gl:GetProcAddress GLvoid "glGetIntegeri_v" GLenum GLuint GLint*))
   (define glEnablei (gl:GetProcAddress GLvoid "glEnablei" GLenum GLuint))
   (define glDisablei (gl:GetProcAddress GLvoid "glDisablei" GLenum GLuint))
   (define glIsEnabledi (gl:GetProcAddress GLboolean "glIsEnabledi" GLenum GLuint))
   (define glBeginTransformFeedback (gl:GetProcAddress GLvoid "glBeginTransformFeedback" GLenum))
   (define glEndTransformFeedback (gl:GetProcAddress GLvoid "glEndTransformFeedback"))
   (define glBindBufferRange (gl:GetProcAddress GLvoid "glBindBufferRange" GLenum GLuint GLuint GLintptr GLsizeiptr))
   (define glBindBufferBase (gl:GetProcAddress GLvoid "glBindBufferBase" GLenum GLuint GLuint))
   (define glTransformFeedbackVaryings (gl:GetProcAddress GLvoid "glTransformFeedbackVaryings" GLuint GLsizei (fft* GLchar*) GLenum)) ; todo: check this
   (define glGetTransformFeedbackVarying (gl:GetProcAddress GLvoid "glGetTransformFeedbackVarying" GLuint GLuint GLsizei (fft& GLsizei) (fft& GLsizei) (fft& GLenum) type-string)) ; todo: check this
   (define glClampColor (gl:GetProcAddress GLvoid "glClampColor" GLenum GLenum))
   (define glBeginConditionalRender (gl:GetProcAddress GLvoid "glBeginConditionalRender" GLuint GLenum))
   (define glEndConditionalRender (gl:GetProcAddress GLvoid "glEndConditionalRender"))
   (define glVertexAttribI1i (gl:GetProcAddress GLvoid "glVertexAttribI1i" GLuint GLint))
   (define glVertexAttribI2i (gl:GetProcAddress GLvoid "glVertexAttribI2i" GLuint GLint GLint))
   (define glVertexAttribI3i (gl:GetProcAddress GLvoid "glVertexAttribI3i" GLuint GLint GLint GLint))
   (define glVertexAttribI4i (gl:GetProcAddress GLvoid "glVertexAttribI4i" GLuint GLint GLint GLint GLint))
   (define glVertexAttribI1ui (gl:GetProcAddress GLvoid "glVertexAttribI1ui" GLuint GLuint))
   (define glVertexAttribI2ui (gl:GetProcAddress GLvoid "glVertexAttribI2ui" GLuint GLuint GLuint))
   (define glVertexAttribI3ui (gl:GetProcAddress GLvoid "glVertexAttribI3ui" GLuint GLuint GLuint GLuint))
   (define glVertexAttribI4ui (gl:GetProcAddress GLvoid "glVertexAttribI4ui" GLuint GLuint GLuint GLuint GLuint))
   (define glVertexAttribI1iv (gl:GetProcAddress GLvoid "glVertexAttribI1iv" GLuint GLint*))
   (define glVertexAttribI2iv (gl:GetProcAddress GLvoid "glVertexAttribI2iv" GLuint GLint*))
   (define glVertexAttribI3iv (gl:GetProcAddress GLvoid "glVertexAttribI3iv" GLuint GLint*))
   (define glVertexAttribI4iv (gl:GetProcAddress GLvoid "glVertexAttribI4iv" GLuint GLint*))
   (define glVertexAttribI1uiv (gl:GetProcAddress GLvoid "glVertexAttribI1uiv" GLuint GLuint*))
   (define glVertexAttribI2uiv (gl:GetProcAddress GLvoid "glVertexAttribI2uiv" GLuint GLuint*))
   (define glVertexAttribI3uiv (gl:GetProcAddress GLvoid "glVertexAttribI3uiv" GLuint GLuint*))
   (define glVertexAttribI4uiv (gl:GetProcAddress GLvoid "glVertexAttribI4uiv" GLuint GLuint*))
   (define glVertexAttribI4bv (gl:GetProcAddress GLvoid "glVertexAttribI4bv" GLuint GLbyte*))
   (define glVertexAttribI4sv (gl:GetProcAddress GLvoid "glVertexAttribI4sv" GLuint GLshort*))
   (define glVertexAttribI4ubv (gl:GetProcAddress GLvoid "glVertexAttribI4ubv" GLuint GLubyte*))
   (define glVertexAttribI4usv (gl:GetProcAddress GLvoid "glVertexAttribI4usv" GLuint GLushort*))
   (define glVertexAttribIPointer (gl:GetProcAddress GLvoid "glVertexAttribIPointer" GLuint GLint GLenum GLsizei GLvoid*))
   (define glGetVertexAttribIiv (gl:GetProcAddress GLvoid "glGetVertexAttribIiv" GLuint GLenum GLint*))
   (define glGetVertexAttribIuiv (gl:GetProcAddress GLvoid "glGetVertexAttribIuiv" GLuint GLenum GLuint*))
   (define glGetUniformuiv (gl:GetProcAddress GLvoid "glGetUniformuiv" GLuint GLint GLuint*))
   (define glBindFragDataLocation (gl:GetProcAddress GLvoid "glBindFragDataLocation" GLuint GLuint GLchar*))
   (define glGetFragDataLocation (gl:GetProcAddress GLint "glGetFragDataLocation" GLuint GLchar*))
   (define glUniform1ui (gl:GetProcAddress GLvoid "glUniform1ui" GLint GLuint))
   (define glUniform2ui (gl:GetProcAddress GLvoid "glUniform2ui" GLint GLuint GLuint))
   (define glUniform3ui (gl:GetProcAddress GLvoid "glUniform3ui" GLint GLuint GLuint GLuint))
   (define glUniform4ui (gl:GetProcAddress GLvoid "glUniform4ui" GLint GLuint GLuint GLuint GLuint))
   (define glUniform1uiv (gl:GetProcAddress GLvoid "glUniform1uiv" GLint GLsizei GLuint*))
   (define glUniform2uiv (gl:GetProcAddress GLvoid "glUniform2uiv" GLint GLsizei GLuint*))
   (define glUniform3uiv (gl:GetProcAddress GLvoid "glUniform3uiv" GLint GLsizei GLuint*))
   (define glUniform4uiv (gl:GetProcAddress GLvoid "glUniform4uiv" GLint GLsizei GLuint*))
   (define glTexParameterIiv (gl:GetProcAddress GLvoid "glTexParameterIiv" GLenum GLenum GLint*))
   (define glTexParameterIuiv (gl:GetProcAddress GLvoid "glTexParameterIuiv" GLenum GLenum GLuint*))
   (define glGetTexParameterIiv (gl:GetProcAddress GLvoid "glGetTexParameterIiv" GLenum GLenum GLint*))
   (define glGetTexParameterIuiv (gl:GetProcAddress GLvoid "glGetTexParameterIuiv" GLenum GLenum GLuint*))
   (define glClearBufferiv (gl:GetProcAddress GLvoid "glClearBufferiv" GLenum GLint GLint*))
   (define glClearBufferuiv (gl:GetProcAddress GLvoid "glClearBufferuiv" GLenum GLint GLuint*))
   (define glClearBufferfv (gl:GetProcAddress GLvoid "glClearBufferfv" GLenum GLint GLfloat*))
   (define glClearBufferfi (gl:GetProcAddress GLvoid "glClearBufferfi" GLenum GLint GLfloat GLint))
   (define glGetStringi (gl:GetProcAddress GLubyte* "glGetStringi" GLenum GLuint))

;; /* OpenGL 3.0 also reuses entry points from these extensions: */
;; /* ARB_framebuffer_object */
;; /* ARB_map_buffer_range */ ??? NOT IN REGISTRY
;; /* ARB_vertex_array_object */

 ; ARB_vertex_array_object
   (define GL_ARB_vertex_array_object 1)
   
   (define GL_VERTEX_ARRAY_BINDING #x85B5)

   (define glBindVertexArray (gl:GetProcAddress GLvoid "glBindVertexArray" GLuint))
   (define glDeleteVertexArrays (gl:GetProcAddress GLvoid "glDeleteVertexArrays" GLsizei GLuint*))
   (define glGenVertexArrays (gl:GetProcAddress GLvoid "glGenVertexArrays" GLsizei GLuint*))
   (define glIsVertexArray (gl:GetProcAddress GLboolean "glIsVertexArray" GLuint))
   
 ; ARB_framebuffer_object
   (define GL_ARB_framebuffer_object 1)

   (define GL_INVALID_FRAMEBUFFER_OPERATION  #x0506)
   (define GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING #x8210)
   (define GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE #x8211)
   (define GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE #x8212)
   (define GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE #x8213)
   (define GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE #x8214)
   (define GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE #x8215)
   (define GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE #x8216)
   (define GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE #x8217)
   (define GL_FRAMEBUFFER_DEFAULT            #x8218)
   (define GL_FRAMEBUFFER_UNDEFINED          #x8219)
   (define GL_DEPTH_STENCIL_ATTACHMENT       #x821A)
   (define GL_INDEX                          #x8222)
   (define GL_MAX_RENDERBUFFER_SIZE          #x84E8)
   (define GL_DEPTH_STENCIL                  #x84F9)
   (define GL_UNSIGNED_INT_24_8              #x84FA)
   (define GL_DEPTH24_STENCIL8               #x88F0)
   (define GL_TEXTURE_STENCIL_SIZE           #x88F1)
   (define GL_FRAMEBUFFER_BINDING            #x8CA6)
   (define GL_DRAW_FRAMEBUFFER_BINDING       GL_FRAMEBUFFER_BINDING)
   (define GL_RENDERBUFFER_BINDING           #x8CA7)
   (define GL_READ_FRAMEBUFFER               #x8CA8)
   (define GL_DRAW_FRAMEBUFFER               #x8CA9)
   (define GL_READ_FRAMEBUFFER_BINDING       #x8CAA)
   (define GL_RENDERBUFFER_SAMPLES           #x8CAB)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE #x8CD0)
   (define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME #x8CD1)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL #x8CD2)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE #x8CD3)
   (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER #x8CD4)
   (define GL_FRAMEBUFFER_COMPLETE           #x8CD5)
   (define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT #x8CD6)
   (define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT #x8CD7)
   (define GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER #x8CDB)
   (define GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER #x8CDC)
   (define GL_FRAMEBUFFER_UNSUPPORTED        #x8CDD)
   (define GL_MAX_COLOR_ATTACHMENTS          #x8CDF)
   (define GL_COLOR_ATTACHMENT0              #x8CE0)
   (define GL_COLOR_ATTACHMENT1              #x8CE1)
   (define GL_COLOR_ATTACHMENT2              #x8CE2)
   (define GL_COLOR_ATTACHMENT3              #x8CE3)
   (define GL_COLOR_ATTACHMENT4              #x8CE4)
   (define GL_COLOR_ATTACHMENT5              #x8CE5)
   (define GL_COLOR_ATTACHMENT6              #x8CE6)
   (define GL_COLOR_ATTACHMENT7              #x8CE7)
   (define GL_COLOR_ATTACHMENT8              #x8CE8)
   (define GL_COLOR_ATTACHMENT9              #x8CE9)
   (define GL_COLOR_ATTACHMENT10             #x8CEA)
   (define GL_COLOR_ATTACHMENT11             #x8CEB)
   (define GL_COLOR_ATTACHMENT12             #x8CEC)
   (define GL_COLOR_ATTACHMENT13             #x8CED)
   (define GL_COLOR_ATTACHMENT14             #x8CEE)
   (define GL_COLOR_ATTACHMENT15             #x8CEF)
   (define GL_DEPTH_ATTACHMENT               #x8D00)
   (define GL_STENCIL_ATTACHMENT             #x8D20)
   (define GL_FRAMEBUFFER                    #x8D40)
   (define GL_RENDERBUFFER                   #x8D41)
   (define GL_RENDERBUFFER_WIDTH             #x8D42)
   (define GL_RENDERBUFFER_HEIGHT            #x8D43)
   (define GL_RENDERBUFFER_INTERNAL_FORMAT   #x8D44)
   (define GL_STENCIL_INDEX1                 #x8D46)
   (define GL_STENCIL_INDEX4                 #x8D47)
   (define GL_STENCIL_INDEX8                 #x8D48)
   (define GL_STENCIL_INDEX16                #x8D49)
   (define GL_RENDERBUFFER_RED_SIZE          #x8D50)
   (define GL_RENDERBUFFER_GREEN_SIZE        #x8D51)
   (define GL_RENDERBUFFER_BLUE_SIZE         #x8D52)
   (define GL_RENDERBUFFER_ALPHA_SIZE        #x8D53)
   (define GL_RENDERBUFFER_DEPTH_SIZE        #x8D54)
   (define GL_RENDERBUFFER_STENCIL_SIZE      #x8D55)
   (define GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE #x8D56)
   (define GL_MAX_SAMPLES                    #x8D57)
      
   (define glIsRenderbuffer (gl:GetProcAddress GLboolean "glIsRenderbuffer" GLuint))
   (define glBindRenderbuffer (gl:GetProcAddress GLvoid "glBindRenderbuffer" GLenum GLuint))
   (define glDeleteRenderbuffers (gl:GetProcAddress GLvoid "glDeleteRenderbuffers" GLsizei GLuint*))
   (define glGenRenderbuffers (gl:GetProcAddress GLvoid "glGenRenderbuffers" GLsizei GLuint*))
   (define glRenderbufferStorage (gl:GetProcAddress GLvoid "glRenderbufferStorage" GLenum GLenum GLsizei GLsizei))
   (define glGetRenderbufferParameteriv (gl:GetProcAddress GLvoid "glGetRenderbufferParameteriv" GLenum GLenum GLint*))
   (define glIsFramebuffer (gl:GetProcAddress GLboolean "glIsFramebuffer" GLuint))
   (define glBindFramebuffer (gl:GetProcAddress GLvoid "glBindFramebuffer" GLenum GLuint))
   (define glDeleteFramebuffers (gl:GetProcAddress GLvoid "glDeleteFramebuffers" GLsizei GLuint*))
   (define glGenFramebuffers (gl:GetProcAddress GLvoid "glGenFramebuffers" GLsizei GLuint*))
   (define glCheckFramebufferStatus (gl:GetProcAddress GLenum "glCheckFramebufferStatus" GLenum))
   (define glFramebufferTexture1D (gl:GetProcAddress GLvoid "glFramebufferTexture1D" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture2D (gl:GetProcAddress GLvoid "glFramebufferTexture2D" GLenum GLenum GLenum GLuint GLint))
   (define glFramebufferTexture3D (gl:GetProcAddress GLvoid "glFramebufferTexture3D" GLenum GLenum GLenum GLuint GLint GLint))
   (define glFramebufferRenderbuffer (gl:GetProcAddress GLvoid "glFramebufferRenderbuffer" GLenum GLenum GLenum GLuint))
   (define glGetFramebufferAttachmentParameteriv (gl:GetProcAddress GLvoid "glGetFramebufferAttachmentParameteriv" GLenum GLenum GLenum GLint*))
   (define glGenerateMipmap (gl:GetProcAddress GLvoid "glGenerateMipmap" GLenum))
   (define glBlitFramebuffer (gl:GetProcAddress GLvoid "glBlitFramebuffer" GLint GLint GLint GLint GLint GLint GLint GLint GLbitfield GLenum))
   (define glRenderbufferStorageMultisample (gl:GetProcAddress GLvoid "glRenderbufferStorageMultisample" GLenum GLsizei GLenum GLsizei GLsizei))
   (define glFramebufferTextureLayer (gl:GetProcAddress GLvoid "glFramebufferTextureLayer" GLenum GLenum GLuint GLint GLint))

))