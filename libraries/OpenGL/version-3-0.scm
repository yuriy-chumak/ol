; OpenGL 3.0 (2008)

(define-library (OpenGL version-3-0)
(export

   GL_VERSION_3_0

   GL_MAJOR_VERSION
   GL_MINOR_VERSION

   gl:CreateContextAttribs

   (exports (OpenGL version-2-1)))

(import (scheme core)
   (OpenGL GLX ARB create_context)
   (OpenGL WGL ARB create_context)
   (OpenGL version-2-1))

(begin
   (define GL_VERSION_3_0 1)

   (define GL_MAJOR_VERSION                  #x821B)
   (define GL_MINOR_VERSION                  #x821C)

   ; tmp
   (define gl:CreateContextAttribs (cond
      (glXCreateContextAttribsARB glXCreateContextAttribsARB)
      (wglCreateContextAttribsARB wglCreateContextAttribsARB)))

#|
#define GL_COMPARE_REF_TO_TEXTURE         GL_COMPARE_R_TO_TEXTURE_ARB
#define GL_CLIP_DISTANCE0                 GL_CLIP_PLANE0
#define GL_CLIP_DISTANCE1                 GL_CLIP_PLANE1
#define GL_CLIP_DISTANCE2                 GL_CLIP_PLANE2
#define GL_CLIP_DISTANCE3                 GL_CLIP_PLANE3
#define GL_CLIP_DISTANCE4                 GL_CLIP_PLANE4
#define GL_CLIP_DISTANCE5                 GL_CLIP_PLANE5
#define GL_MAX_CLIP_DISTANCES             GL_MAX_CLIP_PLANES
#define GL_MAJOR_VERSION                  0x821B
#define GL_MINOR_VERSION                  0x821C
#define GL_NUM_EXTENSIONS                 0x821D
#define GL_CONTEXT_FLAGS                  0x821E
#define GL_DEPTH_BUFFER                   0x8223
#define GL_STENCIL_BUFFER                 0x8224
#define GL_COMPRESSED_RED                 0x8225
#define GL_COMPRESSED_RG                  0x8226
#define GL_CONTEXT_FLAG_FORWARD_COMPATIBLE_BIT 0x0001
#define GL_RGBA32F                        0x8814
#define GL_RGB32F                         0x8815
#define GL_RGBA16F                        0x881A
#define GL_RGB16F                         0x881B
#define GL_VERTEX_ATTRIB_ARRAY_INTEGER    0x88FD
#define GL_MAX_ARRAY_TEXTURE_LAYERS       0x88FF
#define GL_MIN_PROGRAM_TEXEL_OFFSET       0x8904
#define GL_MAX_PROGRAM_TEXEL_OFFSET       0x8905
#define GL_CLAMP_VERTEX_COLOR             0x891A
#define GL_CLAMP_FRAGMENT_COLOR           0x891B
#define GL_CLAMP_READ_COLOR               0x891C
#define GL_FIXED_ONLY                     0x891D
#define GL_MAX_VARYING_COMPONENTS         GL_MAX_VARYING_FLOATS
#define GL_TEXTURE_RED_TYPE               0x8C10
#define GL_TEXTURE_GREEN_TYPE             0x8C11
#define GL_TEXTURE_BLUE_TYPE              0x8C12
#define GL_TEXTURE_ALPHA_TYPE             0x8C13
#define GL_TEXTURE_LUMINANCE_TYPE         0x8C14
#define GL_TEXTURE_INTENSITY_TYPE         0x8C15
#define GL_TEXTURE_DEPTH_TYPE             0x8C16
#define GL_UNSIGNED_NORMALIZED            0x8C17
#define GL_TEXTURE_1D_ARRAY               0x8C18
#define GL_PROXY_TEXTURE_1D_ARRAY         0x8C19
#define GL_TEXTURE_2D_ARRAY               0x8C1A
#define GL_PROXY_TEXTURE_2D_ARRAY         0x8C1B
#define GL_TEXTURE_BINDING_1D_ARRAY       0x8C1C
#define GL_TEXTURE_BINDING_2D_ARRAY       0x8C1D
#define GL_R11F_G11F_B10F                 0x8C3A
#define GL_UNSIGNED_INT_10F_11F_11F_REV   0x8C3B
#define GL_RGB9_E5                        0x8C3D
#define GL_UNSIGNED_INT_5_9_9_9_REV       0x8C3E
#define GL_TEXTURE_SHARED_SIZE            0x8C3F
#define GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH 0x8C76
#define GL_TRANSFORM_FEEDBACK_BUFFER_MODE 0x8C7F
#define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS 0x8C80
#define GL_TRANSFORM_FEEDBACK_VARYINGS    0x8C83
#define GL_TRANSFORM_FEEDBACK_BUFFER_START 0x8C84
#define GL_TRANSFORM_FEEDBACK_BUFFER_SIZE 0x8C85
#define GL_PRIMITIVES_GENERATED           0x8C87
#define GL_TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN 0x8C88
#define GL_RASTERIZER_DISCARD             0x8C89
#define GL_MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS 0x8C8A
#define GL_MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS 0x8C8B
#define GL_INTERLEAVED_ATTRIBS            0x8C8C
#define GL_SEPARATE_ATTRIBS               0x8C8D
#define GL_TRANSFORM_FEEDBACK_BUFFER      0x8C8E
#define GL_TRANSFORM_FEEDBACK_BUFFER_BINDING 0x8C8F
#define GL_RGBA32UI                       0x8D70
#define GL_RGB32UI                        0x8D71
#define GL_RGBA16UI                       0x8D76
#define GL_RGB16UI                        0x8D77
#define GL_RGBA8UI                        0x8D7C
#define GL_RGB8UI                         0x8D7D
#define GL_RGBA32I                        0x8D82
#define GL_RGB32I                         0x8D83
#define GL_RGBA16I                        0x8D88
#define GL_RGB16I                         0x8D89
#define GL_RGBA8I                         0x8D8E
#define GL_RGB8I                          0x8D8F
#define GL_RED_INTEGER                    0x8D94
#define GL_GREEN_INTEGER                  0x8D95
#define GL_BLUE_INTEGER                   0x8D96
#define GL_ALPHA_INTEGER                  0x8D97
#define GL_RGB_INTEGER                    0x8D98
#define GL_RGBA_INTEGER                   0x8D99
#define GL_BGR_INTEGER                    0x8D9A
#define GL_BGRA_INTEGER                   0x8D9B
#define GL_SAMPLER_1D_ARRAY               0x8DC0
#define GL_SAMPLER_2D_ARRAY               0x8DC1
#define GL_SAMPLER_1D_ARRAY_SHADOW        0x8DC3
#define GL_SAMPLER_2D_ARRAY_SHADOW        0x8DC4
#define GL_SAMPLER_CUBE_SHADOW            0x8DC5
#define GL_UNSIGNED_INT_VEC2              0x8DC6
#define GL_UNSIGNED_INT_VEC3              0x8DC7
#define GL_UNSIGNED_INT_VEC4              0x8DC8
#define GL_INT_SAMPLER_1D                 0x8DC9
#define GL_INT_SAMPLER_2D                 0x8DCA
#define GL_INT_SAMPLER_3D                 0x8DCB
#define GL_INT_SAMPLER_CUBE               0x8DCC
#define GL_INT_SAMPLER_1D_ARRAY           0x8DCE
#define GL_INT_SAMPLER_2D_ARRAY           0x8DCF
#define GL_UNSIGNED_INT_SAMPLER_1D        0x8DD1
#define GL_UNSIGNED_INT_SAMPLER_2D        0x8DD2
#define GL_UNSIGNED_INT_SAMPLER_3D        0x8DD3
#define GL_UNSIGNED_INT_SAMPLER_CUBE      0x8DD4
#define GL_UNSIGNED_INT_SAMPLER_1D_ARRAY  0x8DD6
#define GL_UNSIGNED_INT_SAMPLER_2D_ARRAY  0x8DD7
#define GL_QUERY_WAIT                     0x8E13
#define GL_QUERY_NO_WAIT                  0x8E14
#define GL_QUERY_BY_REGION_WAIT           0x8E15
#define GL_QUERY_BY_REGION_NO_WAIT        0x8E16
/* Reuse tokens from ARB_depth_buffer_float */
/* reuse GL_DEPTH_COMPONENT32F */
/* reuse GL_DEPTH32F_STENCIL8 */
/* reuse GL_FLOAT_32_UNSIGNED_INT_24_8_REV */
/* Reuse tokens from ARB_framebuffer_object */
/* reuse GL_INVALID_FRAMEBUFFER_OPERATION */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_RED_SIZE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_GREEN_SIZE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_BLUE_SIZE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE */
/* reuse GL_FRAMEBUFFER_DEFAULT */
/* reuse GL_FRAMEBUFFER_UNDEFINED */
/* reuse GL_DEPTH_STENCIL_ATTACHMENT */
/* reuse GL_INDEX */
/* reuse GL_MAX_RENDERBUFFER_SIZE */
/* reuse GL_DEPTH_STENCIL */
/* reuse GL_UNSIGNED_INT_24_8 */
/* reuse GL_DEPTH24_STENCIL8 */
/* reuse GL_TEXTURE_STENCIL_SIZE */
/* reuse GL_TEXTURE_RED_TYPE */
/* reuse GL_TEXTURE_GREEN_TYPE */
/* reuse GL_TEXTURE_BLUE_TYPE */
/* reuse GL_TEXTURE_ALPHA_TYPE */
/* reuse GL_TEXTURE_LUMINANCE_TYPE */
/* reuse GL_TEXTURE_INTENSITY_TYPE */
/* reuse GL_TEXTURE_DEPTH_TYPE */
/* reuse GL_UNSIGNED_NORMALIZED */
/* reuse GL_FRAMEBUFFER_BINDING */
/* reuse GL_DRAW_FRAMEBUFFER_BINDING */
/* reuse GL_RENDERBUFFER_BINDING */
/* reuse GL_READ_FRAMEBUFFER */
/* reuse GL_DRAW_FRAMEBUFFER */
/* reuse GL_READ_FRAMEBUFFER_BINDING */
/* reuse GL_RENDERBUFFER_SAMPLES */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE */
/* reuse GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER */
/* reuse GL_FRAMEBUFFER_COMPLETE */
/* reuse GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT */
/* reuse GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT */
/* reuse GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER */
/* reuse GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER */
/* reuse GL_FRAMEBUFFER_UNSUPPORTED */
/* reuse GL_MAX_COLOR_ATTACHMENTS */
/* reuse GL_COLOR_ATTACHMENT0 */
/* reuse GL_COLOR_ATTACHMENT1 */
/* reuse GL_COLOR_ATTACHMENT2 */
/* reuse GL_COLOR_ATTACHMENT3 */
/* reuse GL_COLOR_ATTACHMENT4 */
/* reuse GL_COLOR_ATTACHMENT5 */
/* reuse GL_COLOR_ATTACHMENT6 */
/* reuse GL_COLOR_ATTACHMENT7 */
/* reuse GL_COLOR_ATTACHMENT8 */
/* reuse GL_COLOR_ATTACHMENT9 */
/* reuse GL_COLOR_ATTACHMENT10 */
/* reuse GL_COLOR_ATTACHMENT11 */
/* reuse GL_COLOR_ATTACHMENT12 */
/* reuse GL_COLOR_ATTACHMENT13 */
/* reuse GL_COLOR_ATTACHMENT14 */
/* reuse GL_COLOR_ATTACHMENT15 */
/* reuse GL_DEPTH_ATTACHMENT */
/* reuse GL_STENCIL_ATTACHMENT */
/* reuse GL_FRAMEBUFFER */
/* reuse GL_RENDERBUFFER */
/* reuse GL_RENDERBUFFER_WIDTH */
/* reuse GL_RENDERBUFFER_HEIGHT */
/* reuse GL_RENDERBUFFER_INTERNAL_FORMAT */
/* reuse GL_STENCIL_INDEX1 */
/* reuse GL_STENCIL_INDEX4 */
/* reuse GL_STENCIL_INDEX8 */
/* reuse GL_STENCIL_INDEX16 */
/* reuse GL_RENDERBUFFER_RED_SIZE */
/* reuse GL_RENDERBUFFER_GREEN_SIZE */
/* reuse GL_RENDERBUFFER_BLUE_SIZE */
/* reuse GL_RENDERBUFFER_ALPHA_SIZE */
/* reuse GL_RENDERBUFFER_DEPTH_SIZE */
/* reuse GL_RENDERBUFFER_STENCIL_SIZE */
/* reuse GL_FRAMEBUFFER_INCOMPLETE_MULTISAMPLE */
/* reuse GL_MAX_SAMPLES */
/* Reuse tokens from ARB_framebuffer_sRGB */
/* reuse GL_FRAMEBUFFER_SRGB */
/* Reuse tokens from ARB_half_float_vertex */
/* reuse GL_HALF_FLOAT */
/* Reuse tokens from ARB_map_buffer_range */
/* reuse GL_MAP_READ_BIT */
/* reuse GL_MAP_WRITE_BIT */
/* reuse GL_MAP_INVALIDATE_RANGE_BIT */
/* reuse GL_MAP_INVALIDATE_BUFFER_BIT */
/* reuse GL_MAP_FLUSH_EXPLICIT_BIT */
/* reuse GL_MAP_UNSYNCHRONIZED_BIT */
/* Reuse tokens from ARB_texture_compression_rgtc */
/* reuse GL_COMPRESSED_RED_RGTC1 */
/* reuse GL_COMPRESSED_SIGNED_RED_RGTC1 */
/* reuse GL_COMPRESSED_RG_RGTC2 */
/* reuse GL_COMPRESSED_SIGNED_RG_RGTC2 */
/* Reuse tokens from ARB_texture_rg */
/* reuse GL_RG */
/* reuse GL_RG_INTEGER */
/* reuse GL_R8 */
/* reuse GL_R16 */
/* reuse GL_RG8 */
/* reuse GL_RG16 */
/* reuse GL_R16F */
/* reuse GL_R32F */
/* reuse GL_RG16F */
/* reuse GL_RG32F */
/* reuse GL_R8I */
/* reuse GL_R8UI */
/* reuse GL_R16I */
/* reuse GL_R16UI */
/* reuse GL_R32I */
/* reuse GL_R32UI */
/* reuse GL_RG8I */
/* reuse GL_RG8UI */
/* reuse GL_RG16I */
/* reuse GL_RG16UI */
/* reuse GL_RG32I */
/* reuse GL_RG32UI */
/* Reuse tokens from ARB_vertex_array_object */
/* reuse GL_VERTEX_ARRAY_BINDING */
|#

#|
/* OpenGL 3.0 also reuses entry points from these extensions: */
/* ARB_framebuffer_object */
/* ARB_map_buffer_range */
/* ARB_vertex_array_object */
#ifdef GL_GLEXT_PROTOTYPES
GLAPI void APIENTRY glColorMaski (GLuint, GLboolean, GLboolean, GLboolean, GLboolean);
GLAPI void APIENTRY glGetBooleani_v (GLenum, GLuint, GLboolean *);
GLAPI void APIENTRY glGetIntegeri_v (GLenum, GLuint, GLint *);
GLAPI void APIENTRY glEnablei (GLenum, GLuint);
GLAPI void APIENTRY glDisablei (GLenum, GLuint);
GLAPI GLboolean APIENTRY glIsEnabledi (GLenum, GLuint);
GLAPI void APIENTRY glBeginTransformFeedback (GLenum);
GLAPI void APIENTRY glEndTransformFeedback (void);
GLAPI void APIENTRY glBindBufferRange (GLenum, GLuint, GLuint, GLintptr, GLsizeiptr);
GLAPI void APIENTRY glBindBufferBase (GLenum, GLuint, GLuint);
GLAPI void APIENTRY glTransformFeedbackVaryings (GLuint, GLsizei, const GLchar* *, GLenum);
GLAPI void APIENTRY glGetTransformFeedbackVarying (GLuint, GLuint, GLsizei, GLsizei *, GLsizei *, GLenum *, GLchar *);
GLAPI void APIENTRY glClampColor (GLenum, GLenum);
GLAPI void APIENTRY glBeginConditionalRender (GLuint, GLenum);
GLAPI void APIENTRY glEndConditionalRender (void);
GLAPI void APIENTRY glVertexAttribI1i (GLuint, GLint);
GLAPI void APIENTRY glVertexAttribI2i (GLuint, GLint, GLint);
GLAPI void APIENTRY glVertexAttribI3i (GLuint, GLint, GLint, GLint);
GLAPI void APIENTRY glVertexAttribI4i (GLuint, GLint, GLint, GLint, GLint);
GLAPI void APIENTRY glVertexAttribI1ui (GLuint, GLuint);
GLAPI void APIENTRY glVertexAttribI2ui (GLuint, GLuint, GLuint);
GLAPI void APIENTRY glVertexAttribI3ui (GLuint, GLuint, GLuint, GLuint);
GLAPI void APIENTRY glVertexAttribI4ui (GLuint, GLuint, GLuint, GLuint, GLuint);
GLAPI void APIENTRY glVertexAttribI1iv (GLuint, const GLint *);
GLAPI void APIENTRY glVertexAttribI2iv (GLuint, const GLint *);
GLAPI void APIENTRY glVertexAttribI3iv (GLuint, const GLint *);
GLAPI void APIENTRY glVertexAttribI4iv (GLuint, const GLint *);
GLAPI void APIENTRY glVertexAttribI1uiv (GLuint, const GLuint *);
GLAPI void APIENTRY glVertexAttribI2uiv (GLuint, const GLuint *);
GLAPI void APIENTRY glVertexAttribI3uiv (GLuint, const GLuint *);
GLAPI void APIENTRY glVertexAttribI4uiv (GLuint, const GLuint *);
GLAPI void APIENTRY glVertexAttribI4bv (GLuint, const GLbyte *);
GLAPI void APIENTRY glVertexAttribI4sv (GLuint, const GLshort *);
GLAPI void APIENTRY glVertexAttribI4ubv (GLuint, const GLubyte *);
GLAPI void APIENTRY glVertexAttribI4usv (GLuint, const GLushort *);
GLAPI void APIENTRY glVertexAttribIPointer (GLuint, GLint, GLenum, GLsizei, const GLvoid *);
GLAPI void APIENTRY glGetVertexAttribIiv (GLuint, GLenum, GLint *);
GLAPI void APIENTRY glGetVertexAttribIuiv (GLuint, GLenum, GLuint *);
GLAPI void APIENTRY glGetUniformuiv (GLuint, GLint, GLuint *);
GLAPI void APIENTRY glBindFragDataLocation (GLuint, GLuint, const GLchar *);
GLAPI GLint APIENTRY glGetFragDataLocation (GLuint, const GLchar *);
GLAPI void APIENTRY glUniform1ui (GLint, GLuint);
GLAPI void APIENTRY glUniform2ui (GLint, GLuint, GLuint);
GLAPI void APIENTRY glUniform3ui (GLint, GLuint, GLuint, GLuint);
GLAPI void APIENTRY glUniform4ui (GLint, GLuint, GLuint, GLuint, GLuint);
GLAPI void APIENTRY glUniform1uiv (GLint, GLsizei, const GLuint *);
GLAPI void APIENTRY glUniform2uiv (GLint, GLsizei, const GLuint *);
GLAPI void APIENTRY glUniform3uiv (GLint, GLsizei, const GLuint *);
GLAPI void APIENTRY glUniform4uiv (GLint, GLsizei, const GLuint *);
GLAPI void APIENTRY glTexParameterIiv (GLenum, GLenum, const GLint *);
GLAPI void APIENTRY glTexParameterIuiv (GLenum, GLenum, const GLuint *);
GLAPI void APIENTRY glGetTexParameterIiv (GLenum, GLenum, GLint *);
GLAPI void APIENTRY glGetTexParameterIuiv (GLenum, GLenum, GLuint *);
GLAPI void APIENTRY glClearBufferiv (GLenum, GLint, const GLint *);
GLAPI void APIENTRY glClearBufferuiv (GLenum, GLint, const GLuint *);
GLAPI void APIENTRY glClearBufferfv (GLenum, GLint, const GLfloat *);
GLAPI void APIENTRY glClearBufferfi (GLenum, GLint, GLfloat, GLint);
GLAPI const GLubyte * APIENTRY glGetStringi (GLenum, GLuint);
|#
))