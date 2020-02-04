; https://www.khronos.org/registry/egl/
(define-library (OpenGL ES version-2-0)
   (export
      GL_ES_VERSION_2_0

      GLES
      (exports (OpenGL ES platform))

      GL_DEPTH_BUFFER_BIT
      GL_STENCIL_BUFFER_BIT
      GL_COLOR_BUFFER_BIT
      GL_FALSE
      GL_TRUE
      GL_POINTS
      GL_LINES
      GL_LINE_LOOP
      GL_LINE_STRIP
      GL_TRIANGLES
      GL_TRIANGLE_STRIP
      GL_TRIANGLE_FAN
      ;GL_ZERO
      ;GL_ONE
      ;GL_SRC_COLOR
      ;GL_ONE_MINUS_SRC_COLOR
      ;GL_SRC_ALPHA
      ;GL_ONE_MINUS_SRC_ALPHA
      ;GL_DST_ALPHA
      ;GL_ONE_MINUS_DST_ALPHA
      ;GL_DST_COLOR
      ;GL_ONE_MINUS_DST_COLOR
      ;GL_SRC_ALPHA_SATURATE
      ;GL_FUNC_ADD
      ;GL_BLEND_EQUATION
      ;GL_BLEND_EQUATION_RGB
      ;GL_BLEND_EQUATION_ALPHA
      ;GL_FUNC_SUBTRACT
      ;GL_FUNC_REVERSE_SUBTRACT
      ;GL_BLEND_DST_RGB
      ;GL_BLEND_SRC_RGB
      ;GL_BLEND_DST_ALPHA
      ;GL_BLEND_SRC_ALPHA
      ;GL_CONSTANT_COLOR
      ;GL_ONE_MINUS_CONSTANT_COLOR
      ;GL_CONSTANT_ALPHA
      ;GL_ONE_MINUS_CONSTANT_ALPHA
      ;GL_BLEND_COLOR
      GL_ARRAY_BUFFER
      ;GL_ELEMENT_ARRAY_BUFFER
      ;GL_ARRAY_BUFFER_BINDING
      ;GL_ELEMENT_ARRAY_BUFFER_BINDING
      GL_STREAM_DRAW
      GL_STATIC_DRAW
      GL_DYNAMIC_DRAW
      ;GL_BUFFER_SIZE
      ;GL_BUFFER_USAGE
      ;GL_CURRENT_VERTEX_ATTRIB
      ;GL_FRONT
      ;GL_BACK
      ;GL_FRONT_AND_BACK
      ;GL_TEXTURE_2D
      ;GL_CULL_FACE
      ;GL_BLEND
      ;GL_DITHER
      ;GL_STENCIL_TEST
      ;GL_DEPTH_TEST
      ;GL_SCISSOR_TEST
      ;GL_POLYGON_OFFSET_FILL
      ;GL_SAMPLE_ALPHA_TO_COVERAGE
      ;GL_SAMPLE_COVERAGE
      ;GL_NO_ERROR
      ;GL_INVALID_ENUM
      ;GL_INVALID_VALUE
      ;GL_INVALID_OPERATION
      ;GL_OUT_OF_MEMORY
      ;GL_CW
      ;GL_CCW
      ;GL_LINE_WIDTH
      ;GL_ALIASED_POINT_SIZE_RANGE
      ;GL_ALIASED_LINE_WIDTH_RANGE
      ;GL_CULL_FACE_MODE
      ;GL_FRONT_FACE
      ;GL_DEPTH_RANGE
      ;GL_DEPTH_WRITEMASK
      ;GL_DEPTH_CLEAR_VALUE
      ;GL_DEPTH_FUNC
      ;GL_STENCIL_CLEAR_VALUE
      ;GL_STENCIL_FUNC
      ;GL_STENCIL_FAIL
      ;GL_STENCIL_PASS_DEPTH_FAIL
      ;GL_STENCIL_PASS_DEPTH_PASS
      ;GL_STENCIL_REF
      ;GL_STENCIL_VALUE_MASK
      ;GL_STENCIL_WRITEMASK
      ;GL_STENCIL_BACK_FUNC
      ;GL_STENCIL_BACK_FAIL
      ;GL_STENCIL_BACK_PASS_DEPTH_FAIL
      ;GL_STENCIL_BACK_PASS_DEPTH_PASS
      ;GL_STENCIL_BACK_REF
      ;GL_STENCIL_BACK_VALUE_MASK
      ;GL_STENCIL_BACK_WRITEMASK
      ;GL_VIEWPORT
      ;GL_SCISSOR_BOX
      ;GL_COLOR_CLEAR_VALUE
      ;GL_COLOR_WRITEMASK
      ;GL_UNPACK_ALIGNMENT
      ;GL_PACK_ALIGNMENT
      ;GL_MAX_TEXTURE_SIZE
      ;GL_MAX_VIEWPORT_DIMS
      ;GL_SUBPIXEL_BITS
      ;GL_RED_BITS
      ;GL_GREEN_BITS
      ;GL_BLUE_BITS
      ;GL_ALPHA_BITS
      ;GL_DEPTH_BITS
      ;GL_STENCIL_BITS
      ;GL_POLYGON_OFFSET_UNITS
      ;GL_POLYGON_OFFSET_FACTOR
      ;GL_TEXTURE_BINDING_2D
      ;GL_SAMPLE_BUFFERS
      ;GL_SAMPLES
      ;GL_SAMPLE_COVERAGE_VALUE
      ;GL_SAMPLE_COVERAGE_INVERT
      ;GL_NUM_COMPRESSED_TEXTURE_FORMATS
      ;GL_COMPRESSED_TEXTURE_FORMATS
      ;GL_DONT_CARE
      ;GL_FASTEST
      ;GL_NICEST
      ;GL_GENERATE_MIPMAP_HINT
      ;GL_BYTE
      ;GL_UNSIGNED_BYTE
      ;GL_SHORT
      ;GL_UNSIGNED_SHORT
      ;GL_INT
      ;GL_UNSIGNED_INT
      GL_FLOAT
      GL_FIXED
      ;GL_DEPTH_COMPONENT
      ;GL_ALPHA
      ;GL_RGB
      ;GL_RGBA
      ;GL_LUMINANCE
      ;GL_LUMINANCE_ALPHA
      ;GL_UNSIGNED_SHORT_4_4_4_4
      ;GL_UNSIGNED_SHORT_5_5_5_1
      ;GL_UNSIGNED_SHORT_5_6_5
      GL_FRAGMENT_SHADER
      GL_VERTEX_SHADER
      ;GL_MAX_VERTEX_ATTRIBS
      ;GL_MAX_VERTEX_UNIFORM_VECTORS
      ;GL_MAX_VARYING_VECTORS
      ;GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS
      ;GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS
      ;GL_MAX_TEXTURE_IMAGE_UNITS
      ;GL_MAX_FRAGMENT_UNIFORM_VECTORS
      ;GL_SHADER_TYPE
      ;GL_DELETE_STATUS
      ;GL_LINK_STATUS
      ;GL_VALIDATE_STATUS
      ;GL_ATTACHED_SHADERS
      ;GL_ACTIVE_UNIFORMS
      ;GL_ACTIVE_UNIFORM_MAX_LENGTH
      ;GL_ACTIVE_ATTRIBUTES
      ;GL_ACTIVE_ATTRIBUTE_MAX_LENGTH
      ;GL_SHADING_LANGUAGE_VERSION
      ;GL_CURRENT_PROGRAM
      ;GL_NEVER
      ;GL_LESS
      ;GL_EQUAL
      ;GL_LEQUAL
      ;GL_GREATER
      ;GL_NOTEQUAL
      ;GL_GEQUAL
      ;GL_ALWAYS
      ;GL_KEEP
      ;GL_REPLACE
      ;GL_INCR
      ;GL_DECR
      ;GL_INVERT
      ;GL_INCR_WRAP
      ;GL_DECR_WRAP
      ;GL_VENDOR
      ;GL_RENDERER
      ;GL_VERSION
      ;GL_EXTENSIONS
      ;GL_NEAREST
      ;GL_LINEAR
      ;GL_NEAREST_MIPMAP_NEAREST
      ;GL_LINEAR_MIPMAP_NEAREST
      ;GL_NEAREST_MIPMAP_LINEAR
      ;GL_LINEAR_MIPMAP_LINEAR
      ;GL_TEXTURE_MAG_FILTER
      ;GL_TEXTURE_MIN_FILTER
      ;GL_TEXTURE_WRAP_S
      ;GL_TEXTURE_WRAP_T
      ;GL_TEXTURE
      ;GL_TEXTURE_CUBE_MAP
      ;GL_TEXTURE_BINDING_CUBE_MAP
      ;GL_TEXTURE_CUBE_MAP_POSITIVE_X
      ;GL_TEXTURE_CUBE_MAP_NEGATIVE_X
      ;GL_TEXTURE_CUBE_MAP_POSITIVE_Y
      ;GL_TEXTURE_CUBE_MAP_NEGATIVE_Y
      ;GL_TEXTURE_CUBE_MAP_POSITIVE_Z
      ;GL_TEXTURE_CUBE_MAP_NEGATIVE_Z
      ;GL_MAX_CUBE_MAP_TEXTURE_SIZE
      ;GL_TEXTURE0
      ;GL_TEXTURE1
      ;GL_TEXTURE2
      ;GL_TEXTURE3
      ;GL_TEXTURE4
      ;GL_TEXTURE5
      ;GL_TEXTURE6
      ;GL_TEXTURE7
      ;GL_TEXTURE8
      ;GL_TEXTURE9
      ;GL_TEXTURE10
      ;GL_TEXTURE11
      ;GL_TEXTURE12
      ;GL_TEXTURE13
      ;GL_TEXTURE14
      ;GL_TEXTURE15
      ;GL_TEXTURE16
      ;GL_TEXTURE17
      ;GL_TEXTURE18
      ;GL_TEXTURE19
      ;GL_TEXTURE20
      ;GL_TEXTURE21
      ;GL_TEXTURE22
      ;GL_TEXTURE23
      ;GL_TEXTURE24
      ;GL_TEXTURE25
      ;GL_TEXTURE26
      ;GL_TEXTURE27
      ;GL_TEXTURE28
      ;GL_TEXTURE29
      ;GL_TEXTURE30
      ;GL_TEXTURE31
      ;GL_ACTIVE_TEXTURE
      ;GL_REPEAT
      ;GL_CLAMP_TO_EDGE
      ;GL_MIRRORED_REPEAT
      ;GL_FLOAT_VEC2
      ;GL_FLOAT_VEC3
      ;GL_FLOAT_VEC4
      ;GL_INT_VEC2
      ;GL_INT_VEC3
      ;GL_INT_VEC4
      ;GL_BOOL
      ;GL_BOOL_VEC2
      ;GL_BOOL_VEC3
      ;GL_BOOL_VEC4
      ;GL_FLOAT_MAT2
      ;GL_FLOAT_MAT3
      ;GL_FLOAT_MAT4
      ;GL_SAMPLER_2D
      ;GL_SAMPLER_CUBE
      ;GL_VERTEX_ATTRIB_ARRAY_ENABLED
      ;GL_VERTEX_ATTRIB_ARRAY_SIZE
      ;GL_VERTEX_ATTRIB_ARRAY_STRIDE
      ;GL_VERTEX_ATTRIB_ARRAY_TYPE
      ;GL_VERTEX_ATTRIB_ARRAY_NORMALIZED
      ;GL_VERTEX_ATTRIB_ARRAY_POINTER
      ;GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING
      ;GL_IMPLEMENTATION_COLOR_READ_TYPE
      ;GL_IMPLEMENTATION_COLOR_READ_FORMAT
      GL_COMPILE_STATUS
      GL_INFO_LOG_LENGTH
      ;GL_SHADER_SOURCE_LENGTH
      ;GL_SHADER_COMPILER
      ;GL_SHADER_BINARY_FORMATS
      ;GL_NUM_SHADER_BINARY_FORMATS
      ;GL_LOW_FLOAT
      ;GL_MEDIUM_FLOAT
      ;GL_HIGH_FLOAT
      ;GL_LOW_INT
      ;GL_MEDIUM_INT
      ;GL_HIGH_INT
      ;GL_FRAMEBUFFER
      ;GL_RENDERBUFFER
      ;GL_RGBA4
      ;GL_RGB5_A1
      ;GL_RGB565
      ;GL_DEPTH_COMPONENT16
      ;GL_STENCIL_INDEX8
      ;GL_RENDERBUFFER_WIDTH
      ;GL_RENDERBUFFER_HEIGHT
      ;GL_RENDERBUFFER_INTERNAL_FORMAT
      ;GL_RENDERBUFFER_RED_SIZE
      ;GL_RENDERBUFFER_GREEN_SIZE
      ;GL_RENDERBUFFER_BLUE_SIZE
      ;GL_RENDERBUFFER_ALPHA_SIZE
      ;GL_RENDERBUFFER_DEPTH_SIZE
      ;GL_RENDERBUFFER_STENCIL_SIZE
      ;GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE
      ;GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME
      ;GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL
      ;GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE
      ;GL_COLOR_ATTACHMENT0
      ;GL_DEPTH_ATTACHMENT
      ;GL_STENCIL_ATTACHMENT
      ;GL_NONE
      ;GL_FRAMEBUFFER_COMPLETE
      ;GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT
      ;GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT
      ;GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS
      ;GL_FRAMEBUFFER_UNSUPPORTED
      ;GL_FRAMEBUFFER_BINDING
      ;GL_RENDERBUFFER_BINDING
      ;GL_MAX_RENDERBUFFER_SIZE
      ;GL_INVALID_FRAMEBUFFER_OPERATION

      ;glActiveTexture ;void (GLenum texture)
      glAttachShader ;void (GLuint program, GLuint shader)
      ;glBindAttribLocation ;void (GLuint program, GLuint index, const GLchar *name)
      glBindBuffer ;void (GLenum target, GLuint buffer)
      ;glBindFramebuffer ;void (GLenum target, GLuint framebuffer)
      ;glBindRenderbuffer ;void (GLenum target, GLuint renderbuffer)
      ;glBindTexture ;void (GLenum target, GLuint texture)
      ;glBlendColor ;void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
      ;glBlendEquation ;void (GLenum mode)
      ;glBlendEquationSeparate ;void (GLenum modeRGB, GLenum modeAlpha)
      ;glBlendFunc ;void (GLenum sfactor, GLenum dfactor)
      ;glBlendFuncSeparate ;void (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha)
      glBufferData ;void (GLenum target, GLsizeiptr size, const void *data, GLenum usage)
      ;glBufferSubData ;void (GLenum target, GLintptr offset, GLsizeiptr size, const void *data)
      ;glCheckFramebufferStatus ;GLenum (GLenum target)
      glClear ;void (GLbitfield mask)
      glClearColor ;void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha)
      ;glClearDepthf ;void (GLfloat d)
      ;glClearStencil ;void (GLint s)
      ;glColorMask ;void (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha)
      glCompileShader ;void (GLuint shader)
      ;glCompressedTexImage2D ;void (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data)
      ;glCompressedTexSubImage2D ;void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data)
      glCopyTexImage2D ;void (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border)
      ;glCopyTexSubImage2D ;void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height)
      glCreateProgram ;GLuint (void)
      glCreateShader ;GLuint (GLenum type)
      ;glCullFace ;void (GLenum mode)
      glDeleteBuffers ;void (GLsizei n, const GLuint *buffers)
      ;glDeleteFramebuffers ;void (GLsizei n, const GLuint *framebuffers)
      ;glDeleteProgram ;void (GLuint program)
      ;glDeleteRenderbuffers ;void (GLsizei n, const GLuint *renderbuffers)
      ;glDeleteShader ;void (GLuint shader)
      ;glDeleteTextures ;void (GLsizei n, const GLuint *textures)
      ;glDepthFunc ;void (GLenum func)
      ;glDepthMask ;void (GLboolean flag)
      ;glDepthRangef ;void (GLfloat n, GLfloat f)
      glDetachShader ;void (GLuint program, GLuint shader)
      ;glDisable ;void (GLenum cap)
      ;glDisableVertexAttribArray ;void (GLuint index)
      glDrawArrays ;void (GLenum mode, GLint first, GLsizei count)
      ;glDrawElements ;void (GLenum mode, GLsizei count, GLenum type, const void *indices)
      ;glEnable ;void (GLenum cap)
      glEnableVertexAttribArray ;void (GLuint index)
      ;glFinish ;void (void)
      ;glFlush ;void (void)
      ;glFramebufferRenderbuffer ;void (GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer)
      ;glFramebufferTexture2D ;void (GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level)
      ;glFrontFace ;void (GLenum mode)
      glGenBuffers ;void (GLsizei n, GLuint *buffers)
      ;glGenerateMipmap ;void (GLenum target)
      ;glGenFramebuffers ;void (GLsizei n, GLuint *framebuffers)
      ;glGenRenderbuffers ;void (GLsizei n, GLuint *renderbuffers)
      ;glGenTextures ;void (GLsizei n, GLuint *textures)
      ;glGetActiveAttrib ;void (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)
      ;glGetActiveUniform ;void (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name)
      ;glGetAttachedShaders ;void (GLuint program, GLsizei maxCount, GLsizei *count, GLuint *shaders)
      glGetAttribLocation ;GLint (GLuint program, const GLchar *name)
      ;glGetBooleanv ;void (GLenum pname, GLboolean *data)
      ;glGetBufferParameteriv ;void (GLenum target, GLenum pname, GLint *params)
      ;glGetError ;GLenum (void)
      ;glGetFloatv ;void (GLenum pname, GLfloat *data)
      ;glGetFramebufferAttachmentParameteriv ;void (GLenum target, GLenum attachment, GLenum pname, GLint *params)
      ;glGetIntegerv ;void (GLenum pname, GLint *data)
      ;glGetProgramiv ;void (GLuint program, GLenum pname, GLint *params)
      ;glGetProgramInfoLog ;void (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog)
      ;glGetRenderbufferParameteriv ;void (GLenum target, GLenum pname, GLint *params)
      glGetShaderiv ;void (GLuint shader, GLenum pname, GLint *params)
      glGetShaderInfoLog ;void (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *infoLog)
      ;glGetShaderPrecisionFormat ;void (GLenum shadertype, GLenum precisiontype, GLint *range, GLint *precision)
      ;glGetShaderSource ;void (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source)
      ;glGetString ;const GLubyte* (GLenum name);
      ;glGetTexParameterfv ;void (GLenum target, GLenum pname, GLfloat *params)
      ;glGetTexParameteriv ;void (GLenum target, GLenum pname, GLint *params)
      ;glGetUniformfv ;void (GLuint program, GLint location, GLfloat *params)
      ;glGetUniformiv ;void (GLuint program, GLint location, GLint *params)
      glGetUniformLocation ;GLint (GLuint program, const GLchar *name)
      ;glGetVertexAttribfv ;void (GLuint index, GLenum pname, GLfloat *params)
      ;glGetVertexAttribiv ;void (GLuint index, GLenum pname, GLint *params)
      ;glGetVertexAttribPointerv ;void (GLuint index, GLenum pname, void **pointer)
      ;glHint ;void (GLenum target, GLenum mode)
      ;glIsBuffer ;GLboolean (GLuint buffer)
      ;glIsEnabled ;GLboolean (GLenum cap)
      ;glIsFramebuffer ;GLboolean (GLuint framebuffer)
      ;glIsProgram ;GLboolean (GLuint program)
      ;glIsRenderbuffer ;GLboolean (GLuint renderbuffer)
      ;glIsShader ;GLboolean (GLuint shader)
      ;glIsTexture ;GLboolean (GLuint texture)
      ;glLineWidth ;void (GLfloat width)
      glLinkProgram ;void (GLuint program)
      ;glPixelStorei ;void (GLenum pname, GLint param)
      ;glPolygonOffset ;void (GLfloat factor, GLfloat units)
      ;glReadPixels ;void (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels)
      ;glReleaseShaderCompiler ;void (void)
      ;glRenderbufferStorage ;void (GLenum target, GLenum internalformat, GLsizei width, GLsizei height)
      ;glSampleCoverage ;void (GLfloat value, GLboolean invert)
      ;glScissor ;void (GLint x, GLint y, GLsizei width, GLsizei height)
      ;glShaderBinary ;void (GLsizei count, const GLuint *shaders, GLenum binaryformat, const void *binary, GLsizei length)
      glShaderSource ;void (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length)
      ;glStencilFunc ;void (GLenum func, GLint ref, GLuint mask)
      ;glStencilFuncSeparate ;void (GLenum face, GLenum func, GLint ref, GLuint mask)
      ;glStencilMask ;void (GLuint mask)
      ;glStencilMaskSeparate ;void (GLenum face, GLuint mask)
      ;glStencilOp ;void (GLenum fail, GLenum zfail, GLenum zpass)
      ;glStencilOpSeparate ;void (GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass)
      ;glTexImage2D ;void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels)
      ;glTexParameterf ;void (GLenum target, GLenum pname, GLfloat param)
      ;glTexParameterfv ;void (GLenum target, GLenum pname, const GLfloat *params)
      ;glTexParameteri ;void (GLenum target, GLenum pname, GLint param)
      ;glTexParameteriv ;void (GLenum target, GLenum pname, const GLint *params)
      ;glTexSubImage2D ;void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels)
      ;glUniform1f ;void (GLint location, GLfloat v0)
      ;glUniform1fv ;void (GLint location, GLsizei count, const GLfloat *value)
      ;glUniform1i ;void (GLint location, GLint v0)
      ;glUniform1iv ;void (GLint location, GLsizei count, const GLint *value)
      ;glUniform2f ;void (GLint location, GLfloat v0, GLfloat v1)
      ;glUniform2fv ;void (GLint location, GLsizei count, const GLfloat *value)
      ;glUniform2i ;void (GLint location, GLint v0, GLint v1)
      ;glUniform2iv ;void (GLint location, GLsizei count, const GLint *value)
      ;glUniform3f ;void (GLint location, GLfloat v0, GLfloat v1, GLfloat v2)
      ;glUniform3fv ;void (GLint location, GLsizei count, const GLfloat *value)
      ;glUniform3i ;void (GLint location, GLint v0, GLint v1, GLint v2)
      ;glUniform3iv ;void (GLint location, GLsizei count, const GLint *value)
      ;glUniform4f ;void (GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3)
      ;glUniform4fv ;void (GLint location, GLsizei count, const GLfloat *value)
      ;glUniform4i ;void (GLint location, GLint v0, GLint v1, GLint v2, GLint v3)
      ;glUniform4iv ;void (GLint location, GLsizei count, const GLint *value)
      ;glUniformMatrix2fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
      ;glUniformMatrix3fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
      glUniformMatrix4fv ;void (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value)
      glUseProgram ;void (GLuint program)
      ;glValidateProgram ;void (GLuint program)
      ;glVertexAttrib1f ;void (GLuint index, GLfloat x)
      ;glVertexAttrib1fv ;void (GLuint index, const GLfloat *v)
      ;glVertexAttrib2f ;void (GLuint index, GLfloat x, GLfloat y)
      ;glVertexAttrib2fv ;void (GLuint index, const GLfloat *v)
      ;glVertexAttrib3f ;void (GLuint index, GLfloat x, GLfloat y, GLfloat z)
      ;glVertexAttrib3fv ;void (GLuint index, const GLfloat *v)
      ;glVertexAttrib4f ;void (GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w)
      ;glVertexAttrib4fv ;void (GLuint index, const GLfloat *v)
      glVertexAttribPointer ;void (GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, const void *pointer)
      ;glViewport ;void (GLint x, GLint y, GLsizei width, GLsizei height)

)

   (import
      (scheme core) (otus ffi)
      (OpenGL ES platform)
      (owl string) (owl interop))

(begin
   (define GL_ES_VERSION_2_0 1)

   (define GLbyte     fft-signed-char)
   (define GLclampf   fft-float)
   (define GLfixed    fft-int32)
   (define GLshort    fft-short)
   (define GLushort   fft-unsigned-short)
   (define GLvoid     fft-void)
   (define GLsync     type-vptr)
   (define GLint64    fft-int64)
   (define GLuint64   fft-uint64)
   (define GLenum     fft-unsigned-int)
   (define GLuint     fft-unsigned-int)   (define GLuint* (fft* GLuint))  (define GLuint& (fft& GLuint))
   (define GLchar     fft-int8)
   (define GLfloat    fft-float)
   (define GLsizeiptr (if (eq? (size nullptr) 8)
                        fft-int64
                        fft-long)) ; khronos_ssize_t
;typedef khronos_intptr_t GLintptr;
   (define GLbitfield fft-unsigned-int)
   (define GLint      fft-int)
   (define GLboolean  fft-unsigned-char)
   (define GLsizei    fft-int)
   (define GLubyte    fft-unsigned-char)

   (define GLvoid*  type-vptr)
   (define GLuint*  (fft* GLuint))
   (define GLubyte* type-string) ; ?
   (define GLfloat*  (fft* GLfloat))

   (define GL_DEPTH_BUFFER_BIT               #x00000100)
   (define GL_STENCIL_BUFFER_BIT             #x00000400)
   (define GL_COLOR_BUFFER_BIT               #x00004000)
   (define GL_FALSE                          0)
   (define GL_TRUE                           1)
   (define GL_POINTS                         #x0000)
   (define GL_LINES                          #x0001)
   (define GL_LINE_LOOP                      #x0002)
   (define GL_LINE_STRIP                     #x0003)
   (define GL_TRIANGLES                      #x0004)
   (define GL_TRIANGLE_STRIP                 #x0005)
   (define GL_TRIANGLE_FAN                   #x0006)
;#define GL_ZERO                           0
;#define GL_ONE                            1
;#define GL_SRC_COLOR                      0x0300
;#define GL_ONE_MINUS_SRC_COLOR            0x0301
;#define GL_SRC_ALPHA                      0x0302
;#define GL_ONE_MINUS_SRC_ALPHA            0x0303
;#define GL_DST_ALPHA                      0x0304
;#define GL_ONE_MINUS_DST_ALPHA            0x0305
;#define GL_DST_COLOR                      0x0306
;#define GL_ONE_MINUS_DST_COLOR            0x0307
;#define GL_SRC_ALPHA_SATURATE             0x0308
;#define GL_FUNC_ADD                       0x8006
;#define GL_BLEND_EQUATION                 0x8009
;#define GL_BLEND_EQUATION_RGB             0x8009
;#define GL_BLEND_EQUATION_ALPHA           0x883D
;#define GL_FUNC_SUBTRACT                  0x800A
;#define GL_FUNC_REVERSE_SUBTRACT          0x800B
;#define GL_BLEND_DST_RGB                  0x80C8
;#define GL_BLEND_SRC_RGB                  0x80C9
;#define GL_BLEND_DST_ALPHA                0x80CA
;#define GL_BLEND_SRC_ALPHA                0x80CB
;#define GL_CONSTANT_COLOR                 0x8001
;#define GL_ONE_MINUS_CONSTANT_COLOR       0x8002
;#define GL_CONSTANT_ALPHA                 0x8003
;#define GL_ONE_MINUS_CONSTANT_ALPHA       0x8004
;#define GL_BLEND_COLOR                    0x8005
(define GL_ARRAY_BUFFER                   #x8892)
;#define GL_ELEMENT_ARRAY_BUFFER           0x8893
;#define GL_ARRAY_BUFFER_BINDING           0x8894
;#define GL_ELEMENT_ARRAY_BUFFER_BINDING   0x8895
(define GL_STREAM_DRAW                    #x88E0)
(define GL_STATIC_DRAW                    #x88E4)
(define GL_DYNAMIC_DRAW                   #x88E8)
;#define GL_BUFFER_SIZE                    0x8764
;#define GL_BUFFER_USAGE                   0x8765
;#define GL_CURRENT_VERTEX_ATTRIB          0x8626
;#define GL_FRONT                          0x0404
;#define GL_BACK                           0x0405
;#define GL_FRONT_AND_BACK                 0x0408
;#define GL_TEXTURE_2D                     0x0DE1
;#define GL_CULL_FACE                      0x0B44
;#define GL_BLEND                          0x0BE2
;#define GL_DITHER                         0x0BD0
;#define GL_STENCIL_TEST                   0x0B90
;#define GL_DEPTH_TEST                     0x0B71
;#define GL_SCISSOR_TEST                   0x0C11
;#define GL_POLYGON_OFFSET_FILL            0x8037
;#define GL_SAMPLE_ALPHA_TO_COVERAGE       0x809E
;#define GL_SAMPLE_COVERAGE                0x80A0
;#define GL_NO_ERROR                       0
;#define GL_INVALID_ENUM                   0x0500
;#define GL_INVALID_VALUE                  0x0501
;#define GL_INVALID_OPERATION              0x0502
;#define GL_OUT_OF_MEMORY                  0x0505
;#define GL_CW                             0x0900
;#define GL_CCW                            0x0901
;#define GL_LINE_WIDTH                     0x0B21
;#define GL_ALIASED_POINT_SIZE_RANGE       0x846D
;#define GL_ALIASED_LINE_WIDTH_RANGE       0x846E
;#define GL_CULL_FACE_MODE                 0x0B45
;#define GL_FRONT_FACE                     0x0B46
;#define GL_DEPTH_RANGE                    0x0B70
;#define GL_DEPTH_WRITEMASK                0x0B72
;#define GL_DEPTH_CLEAR_VALUE              0x0B73
;#define GL_DEPTH_FUNC                     0x0B74
;#define GL_STENCIL_CLEAR_VALUE            0x0B91
;#define GL_STENCIL_FUNC                   0x0B92
;#define GL_STENCIL_FAIL                   0x0B94
;#define GL_STENCIL_PASS_DEPTH_FAIL        0x0B95
;#define GL_STENCIL_PASS_DEPTH_PASS        0x0B96
;#define GL_STENCIL_REF                    0x0B97
;#define GL_STENCIL_VALUE_MASK             0x0B93
;#define GL_STENCIL_WRITEMASK              0x0B98
;#define GL_STENCIL_BACK_FUNC              0x8800
;#define GL_STENCIL_BACK_FAIL              0x8801
;#define GL_STENCIL_BACK_PASS_DEPTH_FAIL   0x8802
;#define GL_STENCIL_BACK_PASS_DEPTH_PASS   0x8803
;#define GL_STENCIL_BACK_REF               0x8CA3
;#define GL_STENCIL_BACK_VALUE_MASK        0x8CA4
;#define GL_STENCIL_BACK_WRITEMASK         0x8CA5
;#define GL_VIEWPORT                       0x0BA2
;#define GL_SCISSOR_BOX                    0x0C10
;#define GL_COLOR_CLEAR_VALUE              0x0C22
;#define GL_COLOR_WRITEMASK                0x0C23
;#define GL_UNPACK_ALIGNMENT               0x0CF5
;#define GL_PACK_ALIGNMENT                 0x0D05
;#define GL_MAX_TEXTURE_SIZE               0x0D33
;#define GL_MAX_VIEWPORT_DIMS              0x0D3A
;#define GL_SUBPIXEL_BITS                  0x0D50
;#define GL_RED_BITS                       0x0D52
;#define GL_GREEN_BITS                     0x0D53
;#define GL_BLUE_BITS                      0x0D54
;#define GL_ALPHA_BITS                     0x0D55
;#define GL_DEPTH_BITS                     0x0D56
;#define GL_STENCIL_BITS                   0x0D57
;#define GL_POLYGON_OFFSET_UNITS           0x2A00
;#define GL_POLYGON_OFFSET_FACTOR          0x8038
;#define GL_TEXTURE_BINDING_2D             0x8069
;#define GL_SAMPLE_BUFFERS                 0x80A8
;#define GL_SAMPLES                        0x80A9
;#define GL_SAMPLE_COVERAGE_VALUE          0x80AA
;#define GL_SAMPLE_COVERAGE_INVERT         0x80AB
;#define GL_NUM_COMPRESSED_TEXTURE_FORMATS 0x86A2
;#define GL_COMPRESSED_TEXTURE_FORMATS     0x86A3
;#define GL_DONT_CARE                      0x1100
;#define GL_FASTEST                        0x1101
;#define GL_NICEST                         0x1102
;#define GL_GENERATE_MIPMAP_HINT           0x8192
;#define GL_BYTE                           0x1400
;#define GL_UNSIGNED_BYTE                  0x1401
;#define GL_SHORT                          0x1402
;#define GL_UNSIGNED_SHORT                 0x1403
;#define GL_INT                            0x1404
;#define GL_UNSIGNED_INT                   0x1405
(define GL_FLOAT                          #x1406)
(define GL_FIXED                          #x140C)
;#define GL_DEPTH_COMPONENT                0x1902
;#define GL_ALPHA                          0x1906
;#define GL_RGB                            0x1907
;#define GL_RGBA                           0x1908
;#define GL_LUMINANCE                      0x1909
;#define GL_LUMINANCE_ALPHA                0x190A
;#define GL_UNSIGNED_SHORT_4_4_4_4         0x8033
;#define GL_UNSIGNED_SHORT_5_5_5_1         0x8034
;#define GL_UNSIGNED_SHORT_5_6_5           0x8363
(define GL_FRAGMENT_SHADER                #x8B30)
(define GL_VERTEX_SHADER                  #x8B31)
;#define GL_MAX_VERTEX_ATTRIBS             0x8869
;#define GL_MAX_VERTEX_UNIFORM_VECTORS     0x8DFB
;#define GL_MAX_VARYING_VECTORS            0x8DFC
;#define GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS 0x8B4D
;#define GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS 0x8B4C
;#define GL_MAX_TEXTURE_IMAGE_UNITS        0x8872
;#define GL_MAX_FRAGMENT_UNIFORM_VECTORS   0x8DFD
;#define GL_SHADER_TYPE                    0x8B4F
;#define GL_DELETE_STATUS                  0x8B80
;#define GL_LINK_STATUS                    0x8B82
;#define GL_VALIDATE_STATUS                0x8B83
;#define GL_ATTACHED_SHADERS               0x8B85
;#define GL_ACTIVE_UNIFORMS                0x8B86
;#define GL_ACTIVE_UNIFORM_MAX_LENGTH      0x8B87
;#define GL_ACTIVE_ATTRIBUTES              0x8B89
;#define GL_ACTIVE_ATTRIBUTE_MAX_LENGTH    0x8B8A
;#define GL_SHADING_LANGUAGE_VERSION       0x8B8C
;#define GL_CURRENT_PROGRAM                0x8B8D
;#define GL_NEVER                          0x0200
;#define GL_LESS                           0x0201
;#define GL_EQUAL                          0x0202
;#define GL_LEQUAL                         0x0203
;#define GL_GREATER                        0x0204
;#define GL_NOTEQUAL                       0x0205
;#define GL_GEQUAL                         0x0206
;#define GL_ALWAYS                         0x0207
;#define GL_KEEP                           0x1E00
;#define GL_REPLACE                        0x1E01
;#define GL_INCR                           0x1E02
;#define GL_DECR                           0x1E03
;#define GL_INVERT                         0x150A
;#define GL_INCR_WRAP                      0x8507
;#define GL_DECR_WRAP                      0x8508
;#define GL_VENDOR                         0x1F00
;#define GL_RENDERER                       0x1F01
;#define GL_VERSION                        0x1F02
;#define GL_EXTENSIONS                     0x1F03
;#define GL_NEAREST                        0x2600
;#define GL_LINEAR                         0x2601
;#define GL_NEAREST_MIPMAP_NEAREST         0x2700
;#define GL_LINEAR_MIPMAP_NEAREST          0x2701
;#define GL_NEAREST_MIPMAP_LINEAR          0x2702
;#define GL_LINEAR_MIPMAP_LINEAR           0x2703
;#define GL_TEXTURE_MAG_FILTER             0x2800
;#define GL_TEXTURE_MIN_FILTER             0x2801
;#define GL_TEXTURE_WRAP_S                 0x2802
;#define GL_TEXTURE_WRAP_T                 0x2803
;#define GL_TEXTURE                        0x1702
;#define GL_TEXTURE_CUBE_MAP               0x8513
;#define GL_TEXTURE_BINDING_CUBE_MAP       0x8514
;#define GL_TEXTURE_CUBE_MAP_POSITIVE_X    0x8515
;#define GL_TEXTURE_CUBE_MAP_NEGATIVE_X    0x8516
;#define GL_TEXTURE_CUBE_MAP_POSITIVE_Y    0x8517
;#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Y    0x8518
;#define GL_TEXTURE_CUBE_MAP_POSITIVE_Z    0x8519
;#define GL_TEXTURE_CUBE_MAP_NEGATIVE_Z    0x851A
;#define GL_MAX_CUBE_MAP_TEXTURE_SIZE      0x851C
;#define GL_TEXTURE0                       0x84C0
;#define GL_TEXTURE1                       0x84C1
;#define GL_TEXTURE2                       0x84C2
;#define GL_TEXTURE3                       0x84C3
;#define GL_TEXTURE4                       0x84C4
;#define GL_TEXTURE5                       0x84C5
;#define GL_TEXTURE6                       0x84C6
;#define GL_TEXTURE7                       0x84C7
;#define GL_TEXTURE8                       0x84C8
;#define GL_TEXTURE9                       0x84C9
;#define GL_TEXTURE10                      0x84CA
;#define GL_TEXTURE11                      0x84CB
;#define GL_TEXTURE12                      0x84CC
;#define GL_TEXTURE13                      0x84CD
;#define GL_TEXTURE14                      0x84CE
;#define GL_TEXTURE15                      0x84CF
;#define GL_TEXTURE16                      0x84D0
;#define GL_TEXTURE17                      0x84D1
;#define GL_TEXTURE18                      0x84D2
;#define GL_TEXTURE19                      0x84D3
;#define GL_TEXTURE20                      0x84D4
;#define GL_TEXTURE21                      0x84D5
;#define GL_TEXTURE22                      0x84D6
;#define GL_TEXTURE23                      0x84D7
;#define GL_TEXTURE24                      0x84D8
;#define GL_TEXTURE25                      0x84D9
;#define GL_TEXTURE26                      0x84DA
;#define GL_TEXTURE27                      0x84DB
;#define GL_TEXTURE28                      0x84DC
;#define GL_TEXTURE29                      0x84DD
;#define GL_TEXTURE30                      0x84DE
;#define GL_TEXTURE31                      0x84DF
;#define GL_ACTIVE_TEXTURE                 0x84E0
;#define GL_REPEAT                         0x2901
;#define GL_CLAMP_TO_EDGE                  0x812F
;#define GL_MIRRORED_REPEAT                0x8370
;#define GL_FLOAT_VEC2                     0x8B50
;#define GL_FLOAT_VEC3                     0x8B51
;#define GL_FLOAT_VEC4                     0x8B52
;#define GL_INT_VEC2                       0x8B53
;#define GL_INT_VEC3                       0x8B54
;#define GL_INT_VEC4                       0x8B55
;#define GL_BOOL                           0x8B56
;#define GL_BOOL_VEC2                      0x8B57
;#define GL_BOOL_VEC3                      0x8B58
;#define GL_BOOL_VEC4                      0x8B59
;#define GL_FLOAT_MAT2                     0x8B5A
;#define GL_FLOAT_MAT3                     0x8B5B
;#define GL_FLOAT_MAT4                     0x8B5C
;#define GL_SAMPLER_2D                     0x8B5E
;#define GL_SAMPLER_CUBE                   0x8B60
;#define GL_VERTEX_ATTRIB_ARRAY_ENABLED    0x8622
;#define GL_VERTEX_ATTRIB_ARRAY_SIZE       0x8623
;#define GL_VERTEX_ATTRIB_ARRAY_STRIDE     0x8624
;#define GL_VERTEX_ATTRIB_ARRAY_TYPE       0x8625
;#define GL_VERTEX_ATTRIB_ARRAY_NORMALIZED 0x886A
;#define GL_VERTEX_ATTRIB_ARRAY_POINTER    0x8645
;#define GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING 0x889F
;#define GL_IMPLEMENTATION_COLOR_READ_TYPE 0x8B9A
;#define GL_IMPLEMENTATION_COLOR_READ_FORMAT 0x8B9B
(define GL_COMPILE_STATUS                 #x8B81)
(define GL_INFO_LOG_LENGTH                #x8B84)
;#define GL_SHADER_SOURCE_LENGTH           0x8B88
;#define GL_SHADER_COMPILER                0x8DFA
;#define GL_SHADER_BINARY_FORMATS          0x8DF8
;#define GL_NUM_SHADER_BINARY_FORMATS      0x8DF9
;#define GL_LOW_FLOAT                      0x8DF0
;#define GL_MEDIUM_FLOAT                   0x8DF1
;#define GL_HIGH_FLOAT                     0x8DF2
;#define GL_LOW_INT                        0x8DF3
;#define GL_MEDIUM_INT                     0x8DF4
;#define GL_HIGH_INT                       0x8DF5
;#define GL_FRAMEBUFFER                    0x8D40
;#define GL_RENDERBUFFER                   0x8D41
;#define GL_RGBA4                          0x8056
;#define GL_RGB5_A1                        0x8057
;#define GL_RGB565                         0x8D62
;#define GL_DEPTH_COMPONENT16              0x81A5
;#define GL_STENCIL_INDEX8                 0x8D48
;#define GL_RENDERBUFFER_WIDTH             0x8D42
;#define GL_RENDERBUFFER_HEIGHT            0x8D43
;#define GL_RENDERBUFFER_INTERNAL_FORMAT   0x8D44
;#define GL_RENDERBUFFER_RED_SIZE          0x8D50
;#define GL_RENDERBUFFER_GREEN_SIZE        0x8D51
;#define GL_RENDERBUFFER_BLUE_SIZE         0x8D52
;#define GL_RENDERBUFFER_ALPHA_SIZE        0x8D53
;#define GL_RENDERBUFFER_DEPTH_SIZE        0x8D54
;#define GL_RENDERBUFFER_STENCIL_SIZE      0x8D55
;#define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE 0x8CD0
;#define GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME 0x8CD1
;#define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL 0x8CD2
;#define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE 0x8CD3
;#define GL_COLOR_ATTACHMENT0              0x8CE0
;#define GL_DEPTH_ATTACHMENT               0x8D00
;#define GL_STENCIL_ATTACHMENT             0x8D20
;#define GL_NONE                           0
;#define GL_FRAMEBUFFER_COMPLETE           0x8CD5
;#define GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT 0x8CD6
;#define GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT 0x8CD7
;#define GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS 0x8CD9
;#define GL_FRAMEBUFFER_UNSUPPORTED        0x8CDD
;#define GL_FRAMEBUFFER_BINDING            0x8CA6
;#define GL_RENDERBUFFER_BINDING           0x8CA7
;#define GL_MAX_RENDERBUFFER_SIZE          0x84E8
;#define GL_INVALID_FRAMEBUFFER_OPERATION  0x0506
)

(cond-expand
   (Android
      (begin
         (define GLES (or
            (load-dynamic-library "libGLESv2.so")
            (runtime-error "No GLESv2 library found." #f)))))
   (Emscripten
      (begin
         (define GLES (load-dynamic-library #f))))
   (Linux
      (begin
         (define GLES (or
            (load-dynamic-library "libEGL.so")
            (runtime-error "No GLES library found." #f)))))
   (else
      (begin (runtime-error "1Unsupported platform:" *uname*))))

(begin

(define GLchar* type-string)
(define GLchar** (fft* GLchar*))
(define GLint* (fft* GLint))
(define GLint& (fft& GLint))
(define GLsizei* (fft* GLsizei))

;GL_APICALL void GL_APIENTRY glActiveTexture (GLenum texture);
(define glAttachShader (GLES GLvoid "glAttachShader" GLuint GLuint))
;GL_APICALL void GL_APIENTRY glBindAttribLocation (GLuint program, GLuint index, const GLchar *name);
(define glBindBuffer (GLES GLvoid "glBindBuffer" GLenum GLuint))
;GL_APICALL void GL_APIENTRY glBindFramebuffer (GLenum target, GLuint framebuffer);
;GL_APICALL void GL_APIENTRY glBindRenderbuffer (GLenum target, GLuint renderbuffer);
;GL_APICALL void GL_APIENTRY glBindTexture (GLenum target, GLuint texture);
;GL_APICALL void GL_APIENTRY glBlendColor (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
;GL_APICALL void GL_APIENTRY glBlendEquation (GLenum mode);
;GL_APICALL void GL_APIENTRY glBlendEquationSeparate (GLenum modeRGB, GLenum modeAlpha);
;GL_APICALL void GL_APIENTRY glBlendFunc (GLenum sfactor, GLenum dfactor);
;GL_APICALL void GL_APIENTRY glBlendFuncSeparate (GLenum sfactorRGB, GLenum dfactorRGB, GLenum sfactorAlpha, GLenum dfactorAlpha);
(define glBufferData (GLES GLvoid "glBufferData" GLenum GLsizeiptr GLfloat* GLenum)) ; NOTE: const GLvoid * changed to GLfloat*
;GL_APICALL void GL_APIENTRY glBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
;GL_APICALL GLenum GL_APIENTRY glCheckFramebufferStatus (GLenum target);
(define glClear (GLES GLvoid "glClear" GLbitfield))
(define glClearColor (GLES GLvoid "glClearColor" GLfloat GLfloat GLfloat GLfloat))
;GL_APICALL void GL_APIENTRY glClearDepthf (GLfloat d);
;GL_APICALL void GL_APIENTRY glClearStencil (GLint s);
;GL_APICALL void GL_APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
(define glCompileShader (GLES GLvoid "glCompileShader" GLuint))
;GL_APICALL void GL_APIENTRY glCompressedTexImage2D (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data);
;GL_APICALL void GL_APIENTRY glCompressedTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data);
(define glCopyTexImage2D (GLES GLvoid "glCopyTexImage2D" GLenum  GLint GLenum GLint GLint GLsizei GLsizei GLint))
;GL_APICALL void GL_APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
(define glCreateProgram (GLES GLuint "glCreateProgram"))
(define glCreateShader (GLES GLuint "glCreateShader" GLenum))
;GL_APICALL void GL_APIENTRY glCullFace (GLenum mode);
(define glDeleteBuffers (GLES GLvoid "glDeleteBuffers" GLsizei GLuint*))
;GL_APICALL void GL_APIENTRY glDeleteFramebuffers (GLsizei n, const GLuint *framebuffers);
;GL_APICALL void GL_APIENTRY glDeleteProgram (GLuint program);
;GL_APICALL void GL_APIENTRY glDeleteRenderbuffers (GLsizei n, const GLuint *renderbuffers);
;GL_APICALL void GL_APIENTRY glDeleteShader (GLuint shader);
;GL_APICALL void GL_APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
;GL_APICALL void GL_APIENTRY glDepthFunc (GLenum func);
;GL_APICALL void GL_APIENTRY glDepthMask (GLboolean flag);
;GL_APICALL void GL_APIENTRY glDepthRangef (GLfloat n, GLfloat f);
(define glDetachShader (GLES GLvoid "glDetachShader" GLuint GLuint))
;GL_APICALL void GL_APIENTRY glDisable (GLenum cap);
;GL_APICALL void GL_APIENTRY glDisableVertexAttribArray (GLuint index);
(define glDrawArrays (GLES GLvoid "glDrawArrays" GLenum GLint GLsizei))
;GL_APICALL void GL_APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const void *indices);
;GL_APICALL void GL_APIENTRY glEnable (GLenum cap);
(define glEnableVertexAttribArray (GLES GLvoid "glEnableVertexAttribArray" GLuint))
;GL_APICALL void GL_APIENTRY glFinish (void);
;GL_APICALL void GL_APIENTRY glFlush (void);
;GL_APICALL void GL_APIENTRY glFramebufferRenderbuffer (GLenum target, GLenum attachment, GLenum renderbuffertarget, GLuint renderbuffer);
;GL_APICALL void GL_APIENTRY glFramebufferTexture2D (GLenum target, GLenum attachment, GLenum textarget, GLuint texture, GLint level);
;GL_APICALL void GL_APIENTRY glFrontFace (GLenum mode);
(define glGenBuffers (GLES GLvoid "glGenBuffers" GLsizei GLuint&))
;GL_APICALL void GL_APIENTRY glGenerateMipmap (GLenum target);
;GL_APICALL void GL_APIENTRY glGenFramebuffers (GLsizei n, GLuint *framebuffers);
;GL_APICALL void GL_APIENTRY glGenRenderbuffers (GLsizei n, GLuint *renderbuffers);
;GL_APICALL void GL_APIENTRY glGenTextures (GLsizei n, GLuint *textures);
;GL_APICALL void GL_APIENTRY glGetActiveAttrib (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
;GL_APICALL void GL_APIENTRY glGetActiveUniform (GLuint program, GLuint index, GLsizei bufSize, GLsizei *length, GLint *size, GLenum *type, GLchar *name);
;GL_APICALL void GL_APIENTRY glGetAttachedShaders (GLuint program, GLsizei maxCount, GLsizei *count, GLuint *shaders);
(define glGetAttribLocation (GLES GLint "glGetAttribLocation" GLuint GLchar*))
;GL_APICALL void GL_APIENTRY glGetBooleanv (GLenum pname, GLboolean *data);
;GL_APICALL void GL_APIENTRY glGetBufferParameteriv (GLenum target, GLenum pname, GLint *params);
;GL_APICALL GLenum GL_APIENTRY glGetError (void);
;GL_APICALL void GL_APIENTRY glGetFloatv (GLenum pname, GLfloat *data);
;GL_APICALL void GL_APIENTRY glGetFramebufferAttachmentParameteriv (GLenum target, GLenum attachment, GLenum pname, GLint *params);
;GL_APICALL void GL_APIENTRY glGetIntegerv (GLenum pname, GLint *data);
;GL_APICALL void GL_APIENTRY glGetProgramiv (GLuint program, GLenum pname, GLint *params);
;GL_APICALL void GL_APIENTRY glGetProgramInfoLog (GLuint program, GLsizei bufSize, GLsizei *length, GLchar *infoLog);
;GL_APICALL void GL_APIENTRY glGetRenderbufferParameteriv (GLenum target, GLenum pname, GLint *params);
(define glGetShaderiv (GLES GLvoid "glGetShaderiv" GLuint GLenum GLint&))
(define glGetShaderInfoLog (GLES GLvoid "glGetShaderInfoLog" GLuint GLsizei GLsizei* GLchar*))
;GL_APICALL void GL_APIENTRY glGetShaderPrecisionFormat (GLenum shadertype, GLenum precisiontype, GLint *range, GLint *precision);
;GL_APICALL void GL_APIENTRY glGetShaderSource (GLuint shader, GLsizei bufSize, GLsizei *length, GLchar *source);
;GL_APICALL const GLubyte *GL_APIENTRY glGetString (GLenum name);
;GL_APICALL void GL_APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
;GL_APICALL void GL_APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
;GL_APICALL void GL_APIENTRY glGetUniformfv (GLuint program, GLint location, GLfloat *params);
;GL_APICALL void GL_APIENTRY glGetUniformiv (GLuint program, GLint location, GLint *params);
(define glGetUniformLocation (GLES GLint "glGetUniformLocation" GLuint type-string))
;GL_APICALL void GL_APIENTRY glGetVertexAttribfv (GLuint index, GLenum pname, GLfloat *params);
;GL_APICALL void GL_APIENTRY glGetVertexAttribiv (GLuint index, GLenum pname, GLint *params);
;GL_APICALL void GL_APIENTRY glGetVertexAttribPointerv (GLuint index, GLenum pname, void **pointer);
;GL_APICALL void GL_APIENTRY glHint (GLenum target, GLenum mode);
;GL_APICALL GLboolean GL_APIENTRY glIsBuffer (GLuint buffer);
;GL_APICALL GLboolean GL_APIENTRY glIsEnabled (GLenum cap);
;GL_APICALL GLboolean GL_APIENTRY glIsFramebuffer (GLuint framebuffer);
;GL_APICALL GLboolean GL_APIENTRY glIsProgram (GLuint program);
;GL_APICALL GLboolean GL_APIENTRY glIsRenderbuffer (GLuint renderbuffer);
;GL_APICALL GLboolean GL_APIENTRY glIsShader (GLuint shader);
;GL_APICALL GLboolean GL_APIENTRY glIsTexture (GLuint texture);
;GL_APICALL void GL_APIENTRY glLineWidth (GLfloat width);
(define glLinkProgram (GLES GLvoid "glLinkProgram" GLuint))
;GL_APICALL void GL_APIENTRY glPixelStorei (GLenum pname, GLint param);
;GL_APICALL void GL_APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;GL_APICALL void GL_APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
;GL_APICALL void GL_APIENTRY glReleaseShaderCompiler (void);
;GL_APICALL void GL_APIENTRY glRenderbufferStorage (GLenum target, GLenum internalformat, GLsizei width, GLsizei height);
;GL_APICALL void GL_APIENTRY glSampleCoverage (GLfloat value, GLboolean invert);
;GL_APICALL void GL_APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
;GL_APICALL void GL_APIENTRY glShaderBinary (GLsizei count, const GLuint *shaders, GLenum binaryformat, const void *binary, GLsizei length);
;GL_APICALL void GL_APIENTRY glShaderSource (GLuint shader, GLsizei count, const GLchar *const*string, const GLint *length);
(define glShaderSource    (GLES GLvoid "glShaderSource" GLuint GLsizei GLchar** GLint*))

;GL_APICALL void GL_APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
;GL_APICALL void GL_APIENTRY glStencilFuncSeparate (GLenum face, GLenum func, GLint ref, GLuint mask);
;GL_APICALL void GL_APIENTRY glStencilMask (GLuint mask);
;GL_APICALL void GL_APIENTRY glStencilMaskSeparate (GLenum face, GLuint mask);
;GL_APICALL void GL_APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
;GL_APICALL void GL_APIENTRY glStencilOpSeparate (GLenum face, GLenum sfail, GLenum dpfail, GLenum dppass);
;GL_APICALL void GL_APIENTRY glTexImage2D (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
;GL_APICALL void GL_APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
;GL_APICALL void GL_APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
;GL_APICALL void GL_APIENTRY glTexParameteri (GLenum target, GLenum pname, GLint param);
;GL_APICALL void GL_APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
;GL_APICALL void GL_APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
;GL_APICALL void GL_APIENTRY glUniform1f (GLint location, GLfloat v0);
;GL_APICALL void GL_APIENTRY glUniform1fv (GLint location, GLsizei count, const GLfloat *value);
;GL_APICALL void GL_APIENTRY glUniform1i (GLint location, GLint v0);
;GL_APICALL void GL_APIENTRY glUniform1iv (GLint location, GLsizei count, const GLint *value);
;GL_APICALL void GL_APIENTRY glUniform2f (GLint location, GLfloat v0, GLfloat v1);
;GL_APICALL void GL_APIENTRY glUniform2fv (GLint location, GLsizei count, const GLfloat *value);
;GL_APICALL void GL_APIENTRY glUniform2i (GLint location, GLint v0, GLint v1);
;GL_APICALL void GL_APIENTRY glUniform2iv (GLint location, GLsizei count, const GLint *value);
;GL_APICALL void GL_APIENTRY glUniform3f (GLint location, GLfloat v0, GLfloat v1, GLfloat v2);
;GL_APICALL void GL_APIENTRY glUniform3fv (GLint location, GLsizei count, const GLfloat *value);
;GL_APICALL void GL_APIENTRY glUniform3i (GLint location, GLint v0, GLint v1, GLint v2);
;GL_APICALL void GL_APIENTRY glUniform3iv (GLint location, GLsizei count, const GLint *value);
;GL_APICALL void GL_APIENTRY glUniform4f (GLint location, GLfloat v0, GLfloat v1, GLfloat v2, GLfloat v3);
;GL_APICALL void GL_APIENTRY glUniform4fv (GLint location, GLsizei count, const GLfloat *value);
;GL_APICALL void GL_APIENTRY glUniform4i (GLint location, GLint v0, GLint v1, GLint v2, GLint v3);
;GL_APICALL void GL_APIENTRY glUniform4iv (GLint location, GLsizei count, const GLint *value);
;GL_APICALL void GL_APIENTRY glUniformMatrix2fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
;GL_APICALL void GL_APIENTRY glUniformMatrix3fv (GLint location, GLsizei count, GLboolean transpose, const GLfloat *value);
(define glUniformMatrix4fv (GLES GLvoid "glUniformMatrix4fv" GLint GLsizei GLboolean GLfloat*))
(define glUseProgram (GLES GLvoid "glUseProgram" GLuint))
;GL_APICALL void GL_APIENTRY glValidateProgram (GLuint program);
;GL_APICALL void GL_APIENTRY glVertexAttrib1f (GLuint index, GLfloat x);
;GL_APICALL void GL_APIENTRY glVertexAttrib1fv (GLuint index, const GLfloat *v);
;GL_APICALL void GL_APIENTRY glVertexAttrib2f (GLuint index, GLfloat x, GLfloat y);
;GL_APICALL void GL_APIENTRY glVertexAttrib2fv (GLuint index, const GLfloat *v);
;GL_APICALL void GL_APIENTRY glVertexAttrib3f (GLuint index, GLfloat x, GLfloat y, GLfloat z);
;GL_APICALL void GL_APIENTRY glVertexAttrib3fv (GLuint index, const GLfloat *v);
;GL_APICALL void GL_APIENTRY glVertexAttrib4f (GLuint index, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
;GL_APICALL void GL_APIENTRY glVertexAttrib4fv (GLuint index, const GLfloat *v);
(define glVertexAttribPointer (GLES GLvoid "glVertexAttribPointer" GLuint GLint GLenum GLboolean GLsizei GLfloat*)) ; TEMP: GLvoid* changed to GLfloat*
;GL_APICALL void GL_APIENTRY glViewport (GLint x, GLint y, GLsizei width, GLsizei height);

))
