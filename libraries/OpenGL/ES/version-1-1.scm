; https://www.khronos.org/registry/egl/
(define-library (OpenGL ES version-1-1)
   (export
      GL_VERSION_ES_CM_1_0
      GL_VERSION_ES_CL_1_0
      GL_VERSION_ES_CM_1_1
      GL_VERSION_ES_CL_1_1

      GLES
      (exports (OpenGL ES platform))

      ; ClearBufferMask
      GL_DEPTH_BUFFER_BIT
      GL_STENCIL_BUFFER_BIT
      GL_COLOR_BUFFER_BIT

      ; Boolean
      GL_FALSE
      GL_TRUE

      ; BeginMode
      GL_POINTS
      GL_LINES
      GL_LINE_LOOP
      GL_LINE_STRIP
      GL_TRIANGLES
      GL_TRIANGLE_STRIP
      GL_TRIANGLE_FAN

      ; AlphaFunction
      ;GL_NEVER
      ;GL_LESS
      ;GL_EQUAL
      ;GL_LEQUAL
      ;GL_GREATER
      ;GL_NOTEQUAL
      ;GL_GEQUAL
      ;GL_ALWAYS

      ; BlendingFactorDest
      GL_ZERO
      GL_ONE
      GL_SRC_COLOR
      GL_ONE_MINUS_SRC_COLOR
      GL_SRC_ALPHA
      GL_ONE_MINUS_SRC_ALPHA
      GL_DST_ALPHA
      GL_ONE_MINUS_DST_ALPHA

      ; BlendingFactorSrc
      GL_ZERO
      GL_ONE
      GL_DST_COLOR
      GL_ONE_MINUS_DST_COLOR
      GL_SRC_ALPHA_SATURATE
      GL_SRC_ALPHA
      GL_ONE_MINUS_SRC_ALPHA
      GL_DST_ALPHA
      GL_ONE_MINUS_DST_ALPHA

      ; ClipPlaneName
      ;GL_CLIP_PLANE0
      ;GL_CLIP_PLANE1
      ;GL_CLIP_PLANE2
      ;GL_CLIP_PLANE3
      ;GL_CLIP_PLANE4
      ;GL_CLIP_PLANE5

      ; ColorMaterialFace
      ;GL_FRONT_AND_BACK

      ; ColorMaterialParameter
      ;GL_AMBIENT_AND_DIFFUSE

      ; ColorPointerType
      GL_UNSIGNED_BYTE
      GL_FLOAT
      GL_FIXED

      ; CullFaceMode
      ;GL_FRONT
      ;GL_BACK
      ;GL_FRONT_AND_BACK

      ; DepthFunction
      ;; /*      GL_NEVER */
      ;; /*      GL_LESS */
      ;; /*      GL_EQUAL */
      ;; /*      GL_LEQUAL */
      ;; /*      GL_GREATER */
      ;; /*      GL_NOTEQUAL */
      ;; /*      GL_GEQUAL */
      ;; /*      GL_ALWAYS */

      ; EnableCap
      ;GL_FOG
      ;GL_LIGHTING
      GL_TEXTURE_2D
      ;GL_CULL_FACE
      ;GL_ALPHA_TEST
      GL_BLEND
      ;GL_COLOR_LOGIC_OP
      GL_DITHER
      ;GL_STENCIL_TEST
      ;GL_DEPTH_TEST
      ;; /*      GL_LIGHT0 */
      ;; /*      GL_LIGHT1 */
      ;; /*      GL_LIGHT2 */
      ;; /*      GL_LIGHT3 */
      ;; /*      GL_LIGHT4 */
      ;; /*      GL_LIGHT5 */
      ;; /*      GL_LIGHT6 */
      ;; /*      GL_LIGHT7 */
      ;GL_POINT_SMOOTH
      ;GL_LINE_SMOOTH
      ;GL_SCISSOR_TEST
      GL_COLOR_MATERIAL
      ;GL_NORMALIZE
      ;GL_RESCALE_NORMAL
      GL_VERTEX_ARRAY
      GL_NORMAL_ARRAY
      GL_COLOR_ARRAY
      GL_TEXTURE_COORD_ARRAY
      ;GL_MULTISAMPLE
      ;GL_SAMPLE_ALPHA_TO_COVERAGE
      ;GL_SAMPLE_ALPHA_TO_ONE
      ;GL_SAMPLE_COVERAGE

      ; ErrorCode
      ;GL_NO_ERROR
      ;GL_INVALID_ENUM
      ;GL_INVALID_VALUE
      ;GL_INVALID_OPERATION
      ;GL_STACK_OVERFLOW
      ;GL_STACK_UNDERFLOW
      ;GL_OUT_OF_MEMORY

      ; FogMode
      ;GL_LINEAR
      ;GL_EXP
      ;GL_EXP2

      ; FogParameter
      ;GL_FOG_DENSITY
      ;GL_FOG_START
      ;GL_FOG_END
      ;GL_FOG_MODE
      ;GL_FOG_COLOR

      ; FrontFaceDirection
      ;GL_CW
      ;GL_CCW

      ; GetPName
      ;GL_CURRENT_COLOR
      ;GL_CURRENT_NORMAL
      ;GL_CURRENT_TEXTURE_COORDS
      ;GL_POINT_SIZE
      ;GL_POINT_SIZE_MIN
      ;GL_POINT_SIZE_MAX
      ;GL_POINT_FADE_THRESHOLD_SIZE
      ;GL_POINT_DISTANCE_ATTENUATION
      ;GL_SMOOTH_POINT_SIZE_RANGE
      ;GL_LINE_WIDTH
      ;GL_SMOOTH_LINE_WIDTH_RANGE
      ;GL_ALIASED_POINT_SIZE_RANGE
      ;GL_ALIASED_LINE_WIDTH_RANGE
      ;GL_CULL_FACE_MODE
      ;GL_FRONT_FACE
      ;GL_SHADE_MODEL
      ;GL_DEPTH_RANGE
      ;GL_DEPTH_WRITEMASK
      ;GL_DEPTH_CLEAR_VALUE
      ;GL_DEPTH_FUNC
      ;GL_STENCIL_CLEAR_VALUE
      ;GL_STENCIL_FUNC
      ;GL_STENCIL_VALUE_MASK
      ;GL_STENCIL_FAIL
      ;GL_STENCIL_PASS_DEPTH_FAIL
      ;GL_STENCIL_PASS_DEPTH_PASS
      ;GL_STENCIL_REF
      ;GL_STENCIL_WRITEMASK
      ;GL_MATRIX_MODE
      ;GL_VIEWPORT
      ;GL_MODELVIEW_STACK_DEPTH
      ;GL_PROJECTION_STACK_DEPTH
      ;GL_TEXTURE_STACK_DEPTH
      ;GL_MODELVIEW_MATRIX
      ;GL_PROJECTION_MATRIX
      ;GL_TEXTURE_MATRIX
      ;GL_ALPHA_TEST_FUNC
      ;GL_ALPHA_TEST_REF
      ;GL_BLEND_DST
      ;GL_BLEND_SRC
      ;GL_LOGIC_OP_MODE
      ;GL_SCISSOR_BOX
      ;GL_COLOR_CLEAR_VALUE
      ;GL_COLOR_WRITEMASK
      ;GL_MAX_LIGHTS
      ;GL_MAX_CLIP_PLANES
      ;GL_MAX_TEXTURE_SIZE
      ;GL_MAX_MODELVIEW_STACK_DEPTH
      ;GL_MAX_PROJECTION_STACK_DEPTH
      ;GL_MAX_TEXTURE_STACK_DEPTH
      ;GL_MAX_VIEWPORT_DIMS
      ;GL_MAX_TEXTURE_UNITS
      ;GL_SUBPIXEL_BITS
      ;GL_RED_BITS
      ;GL_GREEN_BITS
      ;GL_BLUE_BITS
      ;GL_ALPHA_BITS
      ;GL_DEPTH_BITS
      ;GL_STENCIL_BITS
      ;GL_POLYGON_OFFSET_UNITS
      ;GL_POLYGON_OFFSET_FILL
      ;GL_POLYGON_OFFSET_FACTOR
      ;GL_TEXTURE_BINDING_2D
      ;GL_VERTEX_ARRAY_SIZE
      ;GL_VERTEX_ARRAY_TYPE
      ;GL_VERTEX_ARRAY_STRIDE
      ;GL_NORMAL_ARRAY_TYPE
      ;GL_NORMAL_ARRAY_STRIDE
      ;GL_COLOR_ARRAY_SIZE
      ;GL_COLOR_ARRAY_TYPE
      ;GL_COLOR_ARRAY_STRIDE
      ;GL_TEXTURE_COORD_ARRAY_SIZE
      ;GL_TEXTURE_COORD_ARRAY_TYPE
      ;GL_TEXTURE_COORD_ARRAY_STRIDE
      ;GL_VERTEX_ARRAY_POINTER
      ;GL_NORMAL_ARRAY_POINTER
      ;GL_COLOR_ARRAY_POINTER
      ;GL_TEXTURE_COORD_ARRAY_POINTER
      ;GL_SAMPLE_BUFFERS
      ;GL_SAMPLES
      ;GL_SAMPLE_COVERAGE_VALUE
      ;GL_SAMPLE_COVERAGE_INVERT

      ; GetTextureParameter
      GL_TEXTURE_MAG_FILTER
      GL_TEXTURE_MIN_FILTER
      GL_TEXTURE_WRAP_S
      GL_TEXTURE_WRAP_T

      ;GL_NUM_COMPRESSED_TEXTURE_FORMATS
      ;GL_COMPRESSED_TEXTURE_FORMATS

      ; HintMode
      ;GL_DONT_CARE
      GL_FASTEST
      GL_NICEST

      ; HintTarget
      GL_PERSPECTIVE_CORRECTION_HINT
      ;GL_POINT_SMOOTH_HINT
      ;GL_LINE_SMOOTH_HINT
      ;GL_FOG_HINT
      ;GL_GENERATE_MIPMAP_HINT

      ; LightModelParameter
      ;GL_LIGHT_MODEL_AMBIENT
      ;GL_LIGHT_MODEL_TWO_SIDE

      ; LightParameter
      ;GL_AMBIENT
      ;GL_DIFFUSE
      ;GL_SPECULAR
      ;GL_POSITION
      ;GL_SPOT_DIRECTION
      ;GL_SPOT_EXPONENT
      ;GL_SPOT_CUTOFF
      ;GL_CONSTANT_ATTENUATION
      ;GL_LINEAR_ATTENUATION
      ;GL_QUADRATIC_ATTENUATION

      ; DataType
      GL_BYTE
      GL_UNSIGNED_BYTE
      GL_SHORT
      GL_UNSIGNED_SHORT
      GL_FLOAT
      GL_FIXED

      ; LogicOp
      ;GL_CLEAR
      ;GL_AND
      ;GL_AND_REVERSE
      ;GL_COPY
      ;GL_AND_INVERTED
      ;GL_NOOP
      ;GL_XOR
      ;GL_OR
      ;GL_NOR
      ;GL_EQUIV
      ;GL_INVERT
      ;GL_OR_REVERSE
      ;GL_COPY_INVERTED
      ;GL_OR_INVERTED
      ;GL_NAND
      ;GL_SET

      ; MaterialFace
      ;GL_FRONT_AND_BACK

      ; MaterialParameter
      ;GL_EMISSION
      ;GL_SHININESS
      ;GL_AMBIENT_AND_DIFFUSE
      ;GL_AMBIENT
      ;GL_DIFFUSE
      ;GL_SPECULAR

      ; MatrixMode
      GL_MODELVIEW
      GL_PROJECTION
      GL_TEXTURE

      ; NormalPointerType
      ;GL_BYTE
      ;GL_SHORT
      ;GL_FLOAT
      ;GL_FIXED

      ; PixelFormat
      GL_ALPHA
      GL_RGB
      GL_RGBA
      GL_LUMINANCE
      GL_LUMINANCE_ALPHA

      ; PixelStoreParameter
      GL_UNPACK_ALIGNMENT
      GL_PACK_ALIGNMENT

      ; PixelType
      GL_UNSIGNED_BYTE
      GL_UNSIGNED_SHORT_4_4_4_4
      GL_UNSIGNED_SHORT_5_5_5_1
      GL_UNSIGNED_SHORT_5_6_5

      ; ShadingModel
      GL_FLAT
      GL_SMOOTH

      ; StencilFunction
      ;; /*      GL_NEVER */
      ;; /*      GL_LESS */
      ;; /*      GL_EQUAL */
      ;; /*      GL_LEQUAL */
      ;; /*      GL_GREATER */
      ;; /*      GL_NOTEQUAL */
      ;; /*      GL_GEQUAL */
      ;; /*      GL_ALWAYS */

      ; StencilOp
      ;GL_ZERO
      ;GL_KEEP
      ;GL_REPLACE
      ;GL_INCR
      ;GL_DECR
      ;GL_INVERT

      ; StringName
      ;GL_VENDOR
      ;GL_RENDERER
      ;GL_VERSION
      ;GL_EXTENSIONS

      ; TexCoordPointerType
      ;; /*      GL_SHORT */
      ;; /*      GL_FLOAT */
      ;; /*      GL_FIXED */
      ;; /*      GL_BYTE */

      ; TextureEnvMode
      ;GL_MODULATE
      ;GL_DECAL
      ;GL_BLEND
      ;GL_ADD
      ;GL_REPLACE

      ; TextureEnvParameter
      ;GL_TEXTURE_ENV_MODE
      ;GL_TEXTURE_ENV_COLOR

      ; TextureEnvTarget
      ;GL_TEXTURE_ENV

      ; TextureMagFilter
      GL_NEAREST
      GL_LINEAR

      ; TextureMinFilter
      GL_NEAREST
      GL_LINEAR
      GL_NEAREST_MIPMAP_NEAREST
      GL_LINEAR_MIPMAP_NEAREST
      GL_NEAREST_MIPMAP_LINEAR
      GL_LINEAR_MIPMAP_LINEAR

      ; TextureParameterName
      GL_TEXTURE_MAG_FILTER
      GL_TEXTURE_MIN_FILTER
      GL_TEXTURE_WRAP_S
      GL_TEXTURE_WRAP_T
      GL_GENERATE_MIPMAP

      ; TextureTarget
      GL_TEXTURE_2D

      ; TextureUnit
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
      ;GL_CLIENT_ACTIVE_TEXTURE

      ; TextureWrapMode
      GL_REPEAT
      GL_CLAMP_TO_EDGE

      ; VertexPointerType
      ;GL_SHORT
      ;GL_FLOAT
      ;GL_FIXED
      ;GL_BYTE

      ; LightName
      ;GL_LIGHT0
      ;GL_LIGHT1
      ;GL_LIGHT2
      ;GL_LIGHT3
      ;GL_LIGHT4
      ;GL_LIGHT5
      ;GL_LIGHT6
      ;GL_LIGHT7

      ; Buffer Objects
      GL_ARRAY_BUFFER
      GL_ELEMENT_ARRAY_BUFFER

      GL_ARRAY_BUFFER_BINDING
      GL_ELEMENT_ARRAY_BUFFER_BINDING
      GL_VERTEX_ARRAY_BUFFER_BINDING
      GL_NORMAL_ARRAY_BUFFER_BINDING
      GL_COLOR_ARRAY_BUFFER_BINDING
      GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING

      GL_STATIC_DRAW
      GL_DYNAMIC_DRAW

      GL_BUFFER_SIZE
      GL_BUFFER_USAGE

      ; Texture combine + dot3
      ;GL_SUBTRACT
      ;GL_COMBINE
      ;GL_COMBINE_RGB
      ;GL_COMBINE_ALPHA
      ;GL_RGB_SCALE
      ;GL_ADD_SIGNED
      ;GL_INTERPOLATE
      ;GL_CONSTANT
      ;GL_PRIMARY_COLOR
      ;GL_PREVIOUS
      ;GL_OPERAND0_RGB
      ;GL_OPERAND1_RGB
      ;GL_OPERAND2_RGB
      ;GL_OPERAND0_ALPHA
      ;GL_OPERAND1_ALPHA
      ;GL_OPERAND2_ALPHA

      ;GL_ALPHA_SCALE

      ;GL_SRC0_RGB
      ;GL_SRC1_RGB
      ;GL_SRC2_RGB
      ;GL_SRC0_ALPHA
      ;GL_SRC1_ALPHA
      ;GL_SRC2_ALPHA

      ;GL_DOT3_RGB
      ;GL_DOT3_RGBA

      ; required OES extension tokens
      ;GL_OES_read_format
      ;GL_OES_compressed_paletted_texture
      ;GL_OES_point_size_array
      ;GL_OES_point_sprite

      ;glAlphaFunc ; void (GLenum func, GLfloat ref);
      glClearColor ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
      ;glClearDepthf ; void (GLfloat d);
      glClipPlanef ; void (GLenum p, const GLfloat *eqn);
      glColor4f ; void (GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
      ;glDepthRangef ; void (GLfloat n, GLfloat f);
      ;glFogf ; void (GLenum pname, GLfloat param);
      ;glFogfv ; void (GLenum pname, const GLfloat *params);
      glFrustumf ; void (GLfloat l, GLfloat r, GLfloat b, GLfloat t, GLfloat n, GLfloat f);
      ;glGetClipPlanef ; void (GLenum plane, GLfloat *equation);
      ;glGetFloatv ; void (GLenum pname, GLfloat *data);
      ;glGetLightfv ; void (GLenum light, GLenum pname, GLfloat *params);
      ;glGetMaterialfv ; void (GLenum face, GLenum pname, GLfloat *params);
      ;glGetTexEnvfv ; void (GLenum target, GLenum pname, GLfloat *params);
      ;glGetTexParameterfv ; void (GLenum target, GLenum pname, GLfloat *params);
      ;glLightModelf ; void (GLenum pname, GLfloat param);
      ;glLightModelfv ; void (GLenum pname, const GLfloat *params);
      ;glLightf ; void (GLenum light, GLenum pname, GLfloat param);
      ;glLightfv ; void (GLenum light, GLenum pname, const GLfloat *params);
      glLineWidth ; void (GLfloat width);
      glLoadMatrixf ; void (const GLfloat *m);
      ;glMaterialf ; void (GLenum face, GLenum pname, GLfloat param);
      ;glMaterialfv ; void (GLenum face, GLenum pname, const GLfloat *params);
      ;glMultMatrixf ; void (const GLfloat *m);
      ;glMultiTexCoord4f ; void (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
      glNormal3f ; void (GLfloat nx, GLfloat ny, GLfloat nz);
      glOrthof ; void (GLfloat l, GLfloat r, GLfloat b, GLfloat t, GLfloat n, GLfloat f);
      ;glPointParameterf ; void (GLenum pname, GLfloat param);
      ;glPointParameterfv ; void (GLenum pname, const GLfloat *params);
      ;glPointSize ; void (GLfloat size);
      ;glPolygonOffset ; void (GLfloat factor, GLfloat units);
      glRotatef ; void (GLfloat angle, GLfloat x, GLfloat y, GLfloat z);
      ;glScalef ; void (GLfloat x, GLfloat y, GLfloat z);
      ;glTexEnvf ; void (GLenum target, GLenum pname, GLfloat param);
      ;glTexEnvfv ; void (GLenum target, GLenum pname, const GLfloat *params);
      ;glTexParameterf ; void (GLenum target, GLenum pname, GLfloat param);
      ;glTexParameterfv ; void (GLenum target, GLenum pname, const GLfloat *params);
      glTranslatef ; void (GLfloat x, GLfloat y, GLfloat z);

      ; Available in both Common and Common-Lite profiles
      ;glActiveTexture ; void (GLenum texture);
      ;glAlphaFuncx ; void (GLenum func, GLfixed ref);
      glBindBuffer ; void (GLenum target, GLuint buffer);
      glBindTexture ; void (GLenum target, GLuint texture);
      glBlendFunc ; void (GLenum sfactor, GLenum dfactor);
      glBufferData ; void (GLenum target, GLsizeiptr size, const void *data, GLenum usage);
      ;glBufferSubData ; void (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
      glClear ; void (GLbitfield mask);
      ;glClearColorx ; void (GLfixed red, GLfixed green, GLfixed blue, GLfixed alpha);
      ;glClearDepthx ; void (GLfixed depth);
      ;glClearStencil ; void (GLint s);
      ;glClientActiveTexture ; void (GLenum texture);
      ;glClipPlanex ; void (GLenum plane, const GLfixed *equation);
      ;glColor4ub ; void (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);
      ;glColor4x ; void (GLfixed red, GLfixed green, GLfixed blue, GLfixed alpha);
      ;glColorMask ; void (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
      glColorPointer ; void (GLint size, GLenum type, GLsizei stride, const void *pointer);
      ;glCompressedTexImage2D ; void (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data);
      ;glCompressedTexSubImage2D ; void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data);
      ;glCopyTexImage2D ; void (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
      ;glCopyTexSubImage2D ; void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
      ;glCullFace ; void (GLenum mode);
      glDeleteBuffers ; void (GLsizei n, const GLuint *buffers);
      glDeleteTextures ; void (GLsizei n, const GLuint *textures);
      ;glDepthFunc ; void (GLenum func);
      ;glDepthMask ; void (GLboolean flag);
      ;glDepthRangex ; void (GLfixed n, GLfixed f);
      glDisable ; void (GLenum cap);
      glDisableClientState ; void (GLenum array);
      glDrawArrays ; void (GLenum mode, GLint first, GLsizei count);
      glDrawElements ; void (GLenum mode, GLsizei count, GLenum type, const void *indices);
      glEnable ; void (GLenum cap);
      glEnableClientState ; void (GLenum array);
      glFinish ; void (void);
      glFlush ; void (void);
      ;glFogx ; void (GLenum pname, GLfixed param);
      ;glFogxv ; void (GLenum pname, const GLfixed *param);
      ;glFrontFace ; void (GLenum mode);
      ;glFrustumx ; void (GLfixed l, GLfixed r, GLfixed b, GLfixed t, GLfixed n, GLfixed f);
      ;glGetBooleanv ; void (GLenum pname, GLboolean *data);
      ;glGetBufferParameteriv ; void (GLenum target, GLenum pname, GLint *params);
      ;glGetClipPlanex ; void (GLenum plane, GLfixed *equation);
      glGenBuffers ; void (GLsizei n, GLuint *buffers);
      glGenTextures ; void (GLsizei n, GLuint *textures);
      glGetError ; GLenum (void);
      ;glGetFixedv ; void (GLenum pname, GLfixed *params);
      ;glGetIntegerv ; void (GLenum pname, GLint *data);
      ;glGetLightxv ; void (GLenum light, GLenum pname, GLfixed *params);
      ;glGetMaterialxv ; void (GLenum face, GLenum pname, GLfixed *params);
      ;glGetPointerv ; void (GLenum pname, void **params);
      glGetString ; const GLubyte* (GLenum name);
      ;glGetTexEnviv ; void (GLenum target, GLenum pname, GLint *params);
      ;glGetTexEnvxv ; void (GLenum target, GLenum pname, GLfixed *params);
      ;glGetTexParameteriv ; void (GLenum target, GLenum pname, GLint *params);
      ;glGetTexParameterxv ; void (GLenum target, GLenum pname, GLfixed *params);
      glHint ; void (GLenum target, GLenum mode);
      ;glIsBuffer ; GLboolean (GLuint buffer);
      ;glIsEnabled ; GLboolean (GLenum cap);
      ;glIsTexture ; GLboolean (GLuint texture);
      ;glLightModelx ; void (GLenum pname, GLfixed param);
      ;glLightModelxv ; void (GLenum pname, const GLfixed *param);
      ;glLightx ; void (GLenum light, GLenum pname, GLfixed param);
      ;glLightxv ; void (GLenum light, GLenum pname, const GLfixed *params);
      ;glLineWidthx ; void (GLfixed width);
      glLoadIdentity ; void (void);
      ;glLoadMatrixx ; void (const GLfixed *m);
      ;glLogicOp ; void (GLenum opcode);
      ;glMaterialx ; void (GLenum face, GLenum pname, GLfixed param);
      ;glMaterialxv ; void (GLenum face, GLenum pname, const GLfixed *param);
      glMatrixMode ; void (GLenum mode);
      ;glMultMatrixx ; void (const GLfixed *m);
      ;glMultiTexCoord4x ; void (GLenum texture, GLfixed s, GLfixed t, GLfixed r, GLfixed q);
      ;glNormal3x ; void (GLfixed nx, GLfixed ny, GLfixed nz);
      ;glNormalPointer ; void (GLenum type, GLsizei stride, const void *pointer);
      ;glOrthox ; void (GLfixed l, GLfixed r, GLfixed b, GLfixed t, GLfixed n, GLfixed f);
      glPixelStorei ; void (GLenum pname, GLint param);
      ;glPointParameterx ; void (GLenum pname, GLfixed param);
      ;glPointParameterxv ; void (GLenum pname, const GLfixed *params);
      ;glPointSizex ; void (GLfixed size);
      ;glPolygonOffsetx ; void (GLfixed factor, GLfixed units);
      glPopMatrix ; void (void);
      glPushMatrix ; void (void);
      ;glReadPixels ; void (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
      ;glRotatex ; void (GLfixed angle, GLfixed x, GLfixed y, GLfixed z);
      ;glSampleCoverage ; void (GLfloat value, GLboolean invert);
      ;glSampleCoveragex ; void (GLclampx value, GLboolean invert);
      ;glScalex ; void (GLfixed x, GLfixed y, GLfixed z);
      ;glScissor ; void (GLint x, GLint y, GLsizei width, GLsizei height);
      glShadeModel ; void (GLenum mode);
      ;glStencilFunc ; void (GLenum func, GLint ref, GLuint mask);
      ;glStencilMask ; void (GLuint mask);
      ;glStencilOp ; void (GLenum fail, GLenum zfail, GLenum zpass);
      glTexCoordPointer ; void (GLint size, GLenum type, GLsizei stride, const void *pointer);
      ;glTexEnvi ; void (GLenum target, GLenum pname, GLint param);
      ;glTexEnvx ; void (GLenum target, GLenum pname, GLfixed param);
      ;glTexEnviv ; void (GLenum target, GLenum pname, const GLint *params);
      ;glTexEnvxv ; void (GLenum target, GLenum pname, const GLfixed *params);
      glTexImage2D ; void (GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height, GLint border, GLenum format, GLenum type, const void *pixels);
      glTexParameteri ; void (GLenum target, GLenum pname, GLint param);
      ;glTexParameterx ; void (GLenum target, GLenum pname, GLfixed param);
      ;glTexParameteriv ; void (GLenum target, GLenum pname, const GLint *params);
      ;glTexParameterxv ; void (GLenum target, GLenum pname, const GLfixed *params);
      glTexSubImage2D ; void (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const void *pixels);
      ;glTranslatex ; void (GLfixed x, GLfixed y, GLfixed z);
      glVertexPointer ; void (GLint size, GLenum type, GLsizei stride, const void *pointer);
      glViewport ; void (GLint x, GLint y, GLsizei width, GLsizei height);

      glBegin glEnd
      glVertex2f glColor3f
      glTexCoord2f
      glOrtho
)

   (import
      (scheme core) (otus ffi); (owl io)
      (OpenGL ES platform)
      (owl string) (otus async))

(begin
   (define GL_VERSION_ES_CM_1_0 1)
   (define GL_VERSION_ES_CL_1_0 1)
   (define GL_VERSION_ES_CM_1_1 1)
   (define GL_VERSION_ES_CL_1_1 1)

   ; constants
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
   (define GL_NEVER                          #x0200)
   (define GL_LESS                           #x0201)
   (define GL_EQUAL                          #x0202)
   (define GL_LEQUAL                         #x0203)
   (define GL_GREATER                        #x0204)
   (define GL_NOTEQUAL                       #x0205)
   (define GL_GEQUAL                         #x0206)
   (define GL_ALWAYS                         #x0207)
   (define GL_ZERO                           0)
   (define GL_ONE                            1)
   (define GL_SRC_COLOR                      #x0300)
   (define GL_ONE_MINUS_SRC_COLOR            #x0301)
   (define GL_SRC_ALPHA                      #x0302)
   (define GL_ONE_MINUS_SRC_ALPHA            #x0303)
   (define GL_DST_ALPHA                      #x0304)
   (define GL_ONE_MINUS_DST_ALPHA            #x0305)
   (define GL_DST_COLOR                      #x0306)
   (define GL_ONE_MINUS_DST_COLOR            #x0307)
   (define GL_SRC_ALPHA_SATURATE             #x0308)
   (define GL_CLIP_PLANE0                    #x3000)
   (define GL_CLIP_PLANE1                    #x3001)
   (define GL_CLIP_PLANE2                    #x3002)
   (define GL_CLIP_PLANE3                    #x3003)
   (define GL_CLIP_PLANE4                    #x3004)
   (define GL_CLIP_PLANE5                    #x3005)
   (define GL_FRONT                          #x0404)
   (define GL_BACK                           #x0405)
   (define GL_FRONT_AND_BACK                 #x0408)
   (define GL_FOG                            #x0B60)
   (define GL_LIGHTING                       #x0B50)
   (define GL_TEXTURE_2D                     #x0DE1)
   (define GL_CULL_FACE                      #x0B44)
   (define GL_ALPHA_TEST                     #x0BC0)
   (define GL_BLEND                          #x0BE2)
   (define GL_COLOR_LOGIC_OP                 #x0BF2)
   (define GL_DITHER                         #x0BD0)
   (define GL_STENCIL_TEST                   #x0B90)
   (define GL_DEPTH_TEST                     #x0B71)
   (define GL_POINT_SMOOTH                   #x0B10)
   (define GL_LINE_SMOOTH                    #x0B20)
   (define GL_SCISSOR_TEST                   #x0C11)
   (define GL_COLOR_MATERIAL                 #x0B57)
   (define GL_NORMALIZE                      #x0BA1)
   (define GL_RESCALE_NORMAL                 #x803A)
   (define GL_VERTEX_ARRAY                   #x8074)
   (define GL_NORMAL_ARRAY                   #x8075)
   (define GL_COLOR_ARRAY                    #x8076)
   (define GL_TEXTURE_COORD_ARRAY            #x8078)
   (define GL_MULTISAMPLE                    #x809D)
   (define GL_SAMPLE_ALPHA_TO_COVERAGE       #x809E)
   (define GL_SAMPLE_ALPHA_TO_ONE            #x809F)
   (define GL_SAMPLE_COVERAGE                #x80A0)
   (define GL_NO_ERROR                       0)
   (define GL_INVALID_ENUM                   #x0500)
   (define GL_INVALID_VALUE                  #x0501)
   (define GL_INVALID_OPERATION              #x0502)
   (define GL_STACK_OVERFLOW                 #x0503)
   (define GL_STACK_UNDERFLOW                #x0504)
   (define GL_OUT_OF_MEMORY                  #x0505)
   (define GL_EXP                            #x0800)
   (define GL_EXP2                           #x0801)
   (define GL_FOG_DENSITY                    #x0B62)
   (define GL_FOG_START                      #x0B63)
   (define GL_FOG_END                        #x0B64)
   (define GL_FOG_MODE                       #x0B65)
   (define GL_FOG_COLOR                      #x0B66)
   (define GL_CW                             #x0900)
   (define GL_CCW                            #x0901)
   (define GL_CURRENT_COLOR                  #x0B00)
   (define GL_CURRENT_NORMAL                 #x0B02)
   (define GL_CURRENT_TEXTURE_COORDS         #x0B03)
   (define GL_POINT_SIZE                     #x0B11)
   (define GL_POINT_SIZE_MIN                 #x8126)
   (define GL_POINT_SIZE_MAX                 #x8127)
   (define GL_POINT_FADE_THRESHOLD_SIZE      #x8128)
   (define GL_POINT_DISTANCE_ATTENUATION     #x8129)
   (define GL_SMOOTH_POINT_SIZE_RANGE        #x0B12)
   (define GL_LINE_WIDTH                     #x0B21)
   (define GL_SMOOTH_LINE_WIDTH_RANGE        #x0B22)
   (define GL_ALIASED_POINT_SIZE_RANGE       #x846D)
   (define GL_ALIASED_LINE_WIDTH_RANGE       #x846E)
   (define GL_CULL_FACE_MODE                 #x0B45)
   (define GL_FRONT_FACE                     #x0B46)
   (define GL_SHADE_MODEL                    #x0B54)
   (define GL_DEPTH_RANGE                    #x0B70)
   (define GL_DEPTH_WRITEMASK                #x0B72)
   (define GL_DEPTH_CLEAR_VALUE              #x0B73)
   (define GL_DEPTH_FUNC                     #x0B74)
   (define GL_STENCIL_CLEAR_VALUE            #x0B91)
   (define GL_STENCIL_FUNC                   #x0B92)
   (define GL_STENCIL_VALUE_MASK             #x0B93)
   (define GL_STENCIL_FAIL                   #x0B94)
   (define GL_STENCIL_PASS_DEPTH_FAIL        #x0B95)
   (define GL_STENCIL_PASS_DEPTH_PASS        #x0B96)
   (define GL_STENCIL_REF                    #x0B97)
   (define GL_STENCIL_WRITEMASK              #x0B98)
   (define GL_MATRIX_MODE                    #x0BA0)
   (define GL_VIEWPORT                       #x0BA2)
   (define GL_MODELVIEW_STACK_DEPTH          #x0BA3)
   (define GL_PROJECTION_STACK_DEPTH         #x0BA4)
   (define GL_TEXTURE_STACK_DEPTH            #x0BA5)
   (define GL_MODELVIEW_MATRIX               #x0BA6)
   (define GL_PROJECTION_MATRIX              #x0BA7)
   (define GL_TEXTURE_MATRIX                 #x0BA8)
   (define GL_ALPHA_TEST_FUNC                #x0BC1)
   (define GL_ALPHA_TEST_REF                 #x0BC2)
   (define GL_BLEND_DST                      #x0BE0)
   (define GL_BLEND_SRC                      #x0BE1)
   (define GL_LOGIC_OP_MODE                  #x0BF0)
   (define GL_SCISSOR_BOX                    #x0C10)
   (define GL_COLOR_CLEAR_VALUE              #x0C22)
   (define GL_COLOR_WRITEMASK                #x0C23)
   (define GL_MAX_LIGHTS                     #x0D31)
   (define GL_MAX_CLIP_PLANES                #x0D32)
   (define GL_MAX_TEXTURE_SIZE               #x0D33)
   (define GL_MAX_MODELVIEW_STACK_DEPTH      #x0D36)
   (define GL_MAX_PROJECTION_STACK_DEPTH     #x0D38)
   (define GL_MAX_TEXTURE_STACK_DEPTH        #x0D39)
   (define GL_MAX_VIEWPORT_DIMS              #x0D3A)
   (define GL_MAX_TEXTURE_UNITS              #x84E2)
   (define GL_SUBPIXEL_BITS                  #x0D50)
   (define GL_RED_BITS                       #x0D52)
   (define GL_GREEN_BITS                     #x0D53)
   (define GL_BLUE_BITS                      #x0D54)
   (define GL_ALPHA_BITS                     #x0D55)
   (define GL_DEPTH_BITS                     #x0D56)
   (define GL_STENCIL_BITS                   #x0D57)
   (define GL_POLYGON_OFFSET_UNITS           #x2A00)
   (define GL_POLYGON_OFFSET_FILL            #x8037)
   (define GL_POLYGON_OFFSET_FACTOR          #x8038)
   (define GL_TEXTURE_BINDING_2D             #x8069)
   (define GL_VERTEX_ARRAY_SIZE              #x807A)
   (define GL_VERTEX_ARRAY_TYPE              #x807B)
   (define GL_VERTEX_ARRAY_STRIDE            #x807C)
   (define GL_NORMAL_ARRAY_TYPE              #x807E)
   (define GL_NORMAL_ARRAY_STRIDE            #x807F)
   (define GL_COLOR_ARRAY_SIZE               #x8081)
   (define GL_COLOR_ARRAY_TYPE               #x8082)
   (define GL_COLOR_ARRAY_STRIDE             #x8083)
   (define GL_TEXTURE_COORD_ARRAY_SIZE       #x8088)
   (define GL_TEXTURE_COORD_ARRAY_TYPE       #x8089)
   (define GL_TEXTURE_COORD_ARRAY_STRIDE     #x808A)
   (define GL_VERTEX_ARRAY_POINTER           #x808E)
   (define GL_NORMAL_ARRAY_POINTER           #x808F)
   (define GL_COLOR_ARRAY_POINTER            #x8090)
   (define GL_TEXTURE_COORD_ARRAY_POINTER    #x8092)
   (define GL_SAMPLE_BUFFERS                 #x80A8)
   (define GL_SAMPLES                        #x80A9)
   (define GL_SAMPLE_COVERAGE_VALUE          #x80AA)
   (define GL_SAMPLE_COVERAGE_INVERT         #x80AB)
   (define GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)
   (define GL_COMPRESSED_TEXTURE_FORMATS     #x86A3)
   (define GL_DONT_CARE                      #x1100)
   (define GL_FASTEST                        #x1101)
   (define GL_NICEST                         #x1102)
   (define GL_PERSPECTIVE_CORRECTION_HINT    #x0C50)
   (define GL_POINT_SMOOTH_HINT              #x0C51)
   (define GL_LINE_SMOOTH_HINT               #x0C52)
   (define GL_FOG_HINT                       #x0C54)
   (define GL_GENERATE_MIPMAP_HINT           #x8192)
   (define GL_LIGHT_MODEL_AMBIENT            #x0B53)
   (define GL_LIGHT_MODEL_TWO_SIDE           #x0B52)
   (define GL_AMBIENT                        #x1200)
   (define GL_DIFFUSE                        #x1201)
   (define GL_SPECULAR                       #x1202)
   (define GL_POSITION                       #x1203)
   (define GL_SPOT_DIRECTION                 #x1204)
   (define GL_SPOT_EXPONENT                  #x1205)
   (define GL_SPOT_CUTOFF                    #x1206)
   (define GL_CONSTANT_ATTENUATION           #x1207)
   (define GL_LINEAR_ATTENUATION             #x1208)
   (define GL_QUADRATIC_ATTENUATION          #x1209)
   (define GL_BYTE                           #x1400)
   (define GL_UNSIGNED_BYTE                  #x1401)
   (define GL_SHORT                          #x1402)
   (define GL_UNSIGNED_SHORT                 #x1403)
   (define GL_FLOAT                          #x1406)
   (define GL_FIXED                          #x140C)
   (define GL_CLEAR                          #x1500)
   (define GL_AND                            #x1501)
   (define GL_AND_REVERSE                    #x1502)
   (define GL_COPY                           #x1503)
   (define GL_AND_INVERTED                   #x1504)
   (define GL_NOOP                           #x1505)
   (define GL_XOR                            #x1506)
   (define GL_OR                             #x1507)
   (define GL_NOR                            #x1508)
   (define GL_EQUIV                          #x1509)
   (define GL_INVERT                         #x150A)
   (define GL_OR_REVERSE                     #x150B)
   (define GL_COPY_INVERTED                  #x150C)
   (define GL_OR_INVERTED                    #x150D)
   (define GL_NAND                           #x150E)
   (define GL_SET                            #x150F)
   (define GL_EMISSION                       #x1600)
   (define GL_SHININESS                      #x1601)
   (define GL_AMBIENT_AND_DIFFUSE            #x1602)
   (define GL_MODELVIEW                      #x1700)
   (define GL_PROJECTION                     #x1701)
   (define GL_TEXTURE                        #x1702)
   (define GL_ALPHA                          #x1906)
   (define GL_RGB                            #x1907)
   (define GL_RGBA                           #x1908)
   (define GL_LUMINANCE                      #x1909)
   (define GL_LUMINANCE_ALPHA                #x190A)
   (define GL_UNPACK_ALIGNMENT               #x0CF5)
   (define GL_PACK_ALIGNMENT                 #x0D05)
   (define GL_UNSIGNED_SHORT_4_4_4_4         #x8033)
   (define GL_UNSIGNED_SHORT_5_5_5_1         #x8034)
   (define GL_UNSIGNED_SHORT_5_6_5           #x8363)
   (define GL_FLAT                           #x1D00)
   (define GL_SMOOTH                         #x1D01)
   (define GL_KEEP                           #x1E00)
   (define GL_REPLACE                        #x1E01)
   (define GL_INCR                           #x1E02)
   (define GL_DECR                           #x1E03)
   (define GL_VENDOR                         #x1F00)
   (define GL_RENDERER                       #x1F01)
   (define GL_VERSION                        #x1F02)
   (define GL_EXTENSIONS                     #x1F03)
   (define GL_MODULATE                       #x2100)
   (define GL_DECAL                          #x2101)
   (define GL_ADD                            #x0104)
   (define GL_TEXTURE_ENV_MODE               #x2200)
   (define GL_TEXTURE_ENV_COLOR              #x2201)
   (define GL_TEXTURE_ENV                    #x2300)
   (define GL_NEAREST                        #x2600)
   (define GL_LINEAR                         #x2601)
   (define GL_NEAREST_MIPMAP_NEAREST         #x2700)
   (define GL_LINEAR_MIPMAP_NEAREST          #x2701)
   (define GL_NEAREST_MIPMAP_LINEAR          #x2702)
   (define GL_LINEAR_MIPMAP_LINEAR           #x2703)
   (define GL_TEXTURE_MAG_FILTER             #x2800)
   (define GL_TEXTURE_MIN_FILTER             #x2801)
   (define GL_TEXTURE_WRAP_S                 #x2802)
   (define GL_TEXTURE_WRAP_T                 #x2803)
   (define GL_GENERATE_MIPMAP                #x8191)
   (define GL_TEXTURE0                       #x84C0)
   (define GL_TEXTURE1                       #x84C1)
   (define GL_TEXTURE2                       #x84C2)
   (define GL_TEXTURE3                       #x84C3)
   (define GL_TEXTURE4                       #x84C4)
   (define GL_TEXTURE5                       #x84C5)
   (define GL_TEXTURE6                       #x84C6)
   (define GL_TEXTURE7                       #x84C7)
   (define GL_TEXTURE8                       #x84C8)
   (define GL_TEXTURE9                       #x84C9)
   (define GL_TEXTURE10                      #x84CA)
   (define GL_TEXTURE11                      #x84CB)
   (define GL_TEXTURE12                      #x84CC)
   (define GL_TEXTURE13                      #x84CD)
   (define GL_TEXTURE14                      #x84CE)
   (define GL_TEXTURE15                      #x84CF)
   (define GL_TEXTURE16                      #x84D0)
   (define GL_TEXTURE17                      #x84D1)
   (define GL_TEXTURE18                      #x84D2)
   (define GL_TEXTURE19                      #x84D3)
   (define GL_TEXTURE20                      #x84D4)
   (define GL_TEXTURE21                      #x84D5)
   (define GL_TEXTURE22                      #x84D6)
   (define GL_TEXTURE23                      #x84D7)
   (define GL_TEXTURE24                      #x84D8)
   (define GL_TEXTURE25                      #x84D9)
   (define GL_TEXTURE26                      #x84DA)
   (define GL_TEXTURE27                      #x84DB)
   (define GL_TEXTURE28                      #x84DC)
   (define GL_TEXTURE29                      #x84DD)
   (define GL_TEXTURE30                      #x84DE)
   (define GL_TEXTURE31                      #x84DF)
   (define GL_ACTIVE_TEXTURE                 #x84E0)
   (define GL_CLIENT_ACTIVE_TEXTURE          #x84E1)
   (define GL_REPEAT                         #x2901)
   (define GL_CLAMP_TO_EDGE                  #x812F)
   (define GL_LIGHT0                         #x4000)
   (define GL_LIGHT1                         #x4001)
   (define GL_LIGHT2                         #x4002)
   (define GL_LIGHT3                         #x4003)
   (define GL_LIGHT4                         #x4004)
   (define GL_LIGHT5                         #x4005)
   (define GL_LIGHT6                         #x4006)
   (define GL_LIGHT7                         #x4007)
   (define GL_ARRAY_BUFFER                   #x8892)
   (define GL_ELEMENT_ARRAY_BUFFER           #x8893)
   (define GL_ARRAY_BUFFER_BINDING           #x8894)
   (define GL_ELEMENT_ARRAY_BUFFER_BINDING   #x8895)
   (define GL_VERTEX_ARRAY_BUFFER_BINDING    #x8896)
   (define GL_NORMAL_ARRAY_BUFFER_BINDING    #x8897)
   (define GL_COLOR_ARRAY_BUFFER_BINDING     #x8898)
   (define GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING #x889A)
   (define GL_STATIC_DRAW                    #x88E4)
   (define GL_DYNAMIC_DRAW                   #x88E8)
   (define GL_BUFFER_SIZE                    #x8764)
   (define GL_BUFFER_USAGE                   #x8765)
   (define GL_SUBTRACT                       #x84E7)
   (define GL_COMBINE                        #x8570)
   (define GL_COMBINE_RGB                    #x8571)
   (define GL_COMBINE_ALPHA                  #x8572)
   (define GL_RGB_SCALE                      #x8573)
   (define GL_ADD_SIGNED                     #x8574)
   (define GL_INTERPOLATE                    #x8575)
   (define GL_CONSTANT                       #x8576)
   (define GL_PRIMARY_COLOR                  #x8577)
   (define GL_PREVIOUS                       #x8578)
   (define GL_OPERAND0_RGB                   #x8590)
   (define GL_OPERAND1_RGB                   #x8591)
   (define GL_OPERAND2_RGB                   #x8592)
   (define GL_OPERAND0_ALPHA                 #x8598)
   (define GL_OPERAND1_ALPHA                 #x8599)
   (define GL_OPERAND2_ALPHA                 #x859A)
   (define GL_ALPHA_SCALE                    #x0D1C)
   (define GL_SRC0_RGB                       #x8580)
   (define GL_SRC1_RGB                       #x8581)
   (define GL_SRC2_RGB                       #x8582)
   (define GL_SRC0_ALPHA                     #x8588)
   (define GL_SRC1_ALPHA                     #x8589)
   (define GL_SRC2_ALPHA                     #x858A)
   (define GL_DOT3_RGB                       #x86AE)
   (define GL_DOT3_RGBA                      #x86AF)
)

(cond-expand
   (Android
      (begin
         (define GLES (or
            (load-dynamic-library "libGLESv1_CM.so")
            (runtime-error "No GLESv1 library found." #f)))))
   (Emscripten
      (begin
         (define GLES (load-dynamic-library #f))))
   (else
      (begin (runtime-error "1Unsupported platform:" *uname*))))

(begin

   ; GL_API void GL_APIENTRY glAlphaFunc (GLenum func, GLfloat ref);
   (define glClearColor (GLES GLvoid "glClearColor" GLfloat GLfloat GLfloat GLfloat))
   ; GL_API void GL_APIENTRY glClearDepthf (GLfloat d);
   (define glClipPlanef (GLES GLvoid "glClipPlanef" GLenum GLfloat*))
   (define glColor4f (GLES GLvoid "glColor4f" GLfloat GLfloat GLfloat GLfloat))
   ; GL_API void GL_APIENTRY glDepthRangef (GLfloat n, GLfloat f);
   ; GL_API void GL_APIENTRY glFogf (GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glFogfv (GLenum pname, const GLfloat *params);
   (define glFrustumf (GLES GLvoid "glFrustumf" GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat))
   ; GL_API void GL_APIENTRY glGetClipPlanef (GLenum plane, GLfloat *equation);
   ; GL_API void GL_APIENTRY glGetFloatv (GLenum pname, GLfloat *data);
   ; GL_API void GL_APIENTRY glGetLightfv (GLenum light, GLenum pname, GLfloat *params);
   ; GL_API void GL_APIENTRY glGetMaterialfv (GLenum face, GLenum pname, GLfloat *params);
   ; GL_API void GL_APIENTRY glGetTexEnvfv (GLenum target, GLenum pname, GLfloat *params);
   ; GL_API void GL_APIENTRY glGetTexParameterfv (GLenum target, GLenum pname, GLfloat *params);
   ; GL_API void GL_APIENTRY glLightModelf (GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glLightModelfv (GLenum pname, const GLfloat *params);
   ; GL_API void GL_APIENTRY glLightf (GLenum light, GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glLightfv (GLenum light, GLenum pname, const GLfloat *params);
   (define glLineWidth (GLES GLvoid "glLineWidth" GLfloat))
   (define glLoadMatrixf (GLES GLvoid "glLoadMatrixf" GLfloat*))
   ; GL_API void GL_APIENTRY glMaterialf (GLenum face, GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glMaterialfv (GLenum face, GLenum pname, const GLfloat *params);
   ; GL_API void GL_APIENTRY glMultMatrixf (const GLfloat *m);
   ; GL_API void GL_APIENTRY glMultiTexCoord4f (GLenum target, GLfloat s, GLfloat t, GLfloat r, GLfloat q);
   (define glNormal3f (GLES GLvoid "glNormal3f" GLfloat GLfloat GLfloat))
   (define glOrthof (GLES GLvoid "glOrthof" GLfloat GLfloat GLfloat GLfloat GLfloat GLfloat))
   ; GL_API void GL_APIENTRY glPointParameterf (GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glPointParameterfv (GLenum pname, const GLfloat *params);
   ; GL_API void GL_APIENTRY glPointSize (GLfloat size);
   ; GL_API void GL_APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
   (define glRotatef (GLES GLvoid "glRotatef" GLfloat GLfloat GLfloat GLfloat))
   ; GL_API void GL_APIENTRY glScalef (GLfloat x, GLfloat y, GLfloat z);
   ; GL_API void GL_APIENTRY glTexEnvf (GLenum target, GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glTexEnvfv (GLenum target, GLenum pname, const GLfloat *params);
   ; GL_API void GL_APIENTRY glTexParameterf (GLenum target, GLenum pname, GLfloat param);
   ; GL_API void GL_APIENTRY glTexParameterfv (GLenum target, GLenum pname, const GLfloat *params);
   (define glTranslatef (GLES GLvoid "glTranslatef" GLfloat GLfloat GLfloat))

   ; GL_API void GL_APIENTRY glActiveTexture (GLenum texture);
   ; GL_API void GL_APIENTRY glAlphaFuncx (GLenum func, GLfixed ref);
   (define glBindBuffer (GLES GLvoid "glBindBuffer" GLenum GLuint))
   (define glBindTexture (GLES GLvoid "glBindTexture" GLenum GLuint))
   (define glBlendFunc (GLES GLvoid "glBlendFunc" GLenum GLenum))
   (define glBufferData (GLES GLvoid "glBufferData" GLenum GLsizeiptr fft-any GLenum))
   ; GL_API void GL_APIENTRY glBufferSubData (GLenum target, GLintptr offset, GLsizeiptr size, const void *data);
   (define glClear (GLES GLvoid "glClear" GLbitfield))
   ; GL_API void GL_APIENTRY glClearColorx (GLfixed red, GLfixed green, GLfixed blue, GLfixed alpha);
   ; GL_API void GL_APIENTRY glClearDepthx (GLfixed depth);
   ; GL_API void GL_APIENTRY glClearStencil (GLint s);
   ; GL_API void GL_APIENTRY glClientActiveTexture (GLenum texture);
   ; GL_API void GL_APIENTRY glClipPlanex (GLenum plane, const GLfixed *equation);
   ; GL_API void GL_APIENTRY glColor4ub (GLubyte red, GLubyte green, GLubyte blue, GLubyte alpha);
   ; GL_API void GL_APIENTRY glColor4x (GLfixed red, GLfixed green, GLfixed blue, GLfixed alpha);
   ; GL_API void GL_APIENTRY glColorMask (GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
   (define glColorPointer (GLES GLvoid "glColorPointer" GLint GLenum GLsizei fft-any))
   ;; (define (glColorPointer size type stride pointer)
   ;;    (case type
   ;;       (GL_FLOAT
   ;;          (glColorPointerf size type stride pointer))
   ;;       (else
   ;;          (runtime-error "oops" type))))


   ; GL_API void GL_APIENTRY glCompressedTexImage2D (GLenum target, GLint level, GLenum internalformat, GLsizei width, GLsizei height, GLint border, GLsizei imageSize, const void *data);
   ; GL_API void GL_APIENTRY glCompressedTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, const void *data);
   ; GL_API void GL_APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalformat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
   ; GL_API void GL_APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
   ; GL_API void GL_APIENTRY glCullFace (GLenum mode);
   (define glDeleteBuffers (GLES GLvoid "glDeleteBuffers" GLsizei GLuint*))
   (define glDeleteTextures (GLES GLvoid "glDeleteTextures" GLsizei GLuint*))
   ; GL_API void GL_APIENTRY glDepthFunc (GLenum func);
   ; GL_API void GL_APIENTRY glDepthMask (GLboolean flag);
   ; GL_API void GL_APIENTRY glDepthRangex (GLfixed n, GLfixed f);
   (define glDisable (GLES GLvoid "glDisable" GLenum))
   (define glDisableClientState (GLES GLvoid "glDisableClientState" GLenum))
   (define glDrawArrays (GLES GLvoid "glDrawArrays" GLenum GLint GLsizei))
   (define glDrawElements (GLES GLvoid "glDrawElements" GLenum GLsizei GLenum fft-any))
   (define glEnable (GLES GLvoid "glEnable" GLenum))
   (define glEnableClientState (GLES GLvoid "glEnableClientState" GLenum))
   (define glFinish (GLES GLvoid "glFinish"))
   (define glFlush (GLES GLvoid "glFlush"))
   ; GL_API void GL_APIENTRY glFogx (GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glFogxv (GLenum pname, const GLfixed *param);
   ; GL_API void GL_APIENTRY glFrontFace (GLenum mode);
   ; GL_API void GL_APIENTRY glFrustumx (GLfixed l, GLfixed r, GLfixed b, GLfixed t, GLfixed n, GLfixed f);
   ; GL_API void GL_APIENTRY glGetBooleanv (GLenum pname, GLboolean *data);
   ; GL_API void GL_APIENTRY glGetBufferParameteriv (GLenum target, GLenum pname, GLint *params);
   ; GL_API void GL_APIENTRY glGetClipPlanex (GLenum plane, GLfixed *equation);
   (define glGenBuffers (GLES GLvoid "glGenBuffers" GLsizei GLuint&))
   (define glGenTextures (GLES GLvoid "glGenTextures" GLsizei GLuint&))
   (define glGetError (GLES GLenum "glGetError"))
   ; GL_API void GL_APIENTRY glGetFixedv (GLenum pname, GLfixed *params);
   ; GL_API void GL_APIENTRY glGetIntegerv (GLenum pname, GLint *data);
   ; GL_API void GL_APIENTRY glGetLightxv (GLenum light, GLenum pname, GLfixed *params);
   ; GL_API void GL_APIENTRY glGetMaterialxv (GLenum face, GLenum pname, GLfixed *params);
   ; GL_API void GL_APIENTRY glGetPointerv (GLenum pname, void **params);
   (define glGetString (GLES type-string "glGetString" GLenum))
   ; GL_API void GL_APIENTRY glGetTexEnviv (GLenum target, GLenum pname, GLint *params);
   ; GL_API void GL_APIENTRY glGetTexEnvxv (GLenum target, GLenum pname, GLfixed *params);
   ; GL_API void GL_APIENTRY glGetTexParameteriv (GLenum target, GLenum pname, GLint *params);
   ; GL_API void GL_APIENTRY glGetTexParameterxv (GLenum target, GLenum pname, GLfixed *params);
   (define glHint (GLES GLvoid "glHint" GLenum GLenum))
   ; GL_API GLboolean GL_APIENTRY glIsBuffer (GLuint buffer);
   ; GL_API GLboolean GL_APIENTRY glIsEnabled (GLenum cap);
   ; GL_API GLboolean GL_APIENTRY glIsTexture (GLuint texture);
   ; GL_API void GL_APIENTRY glLightModelx (GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glLightModelxv (GLenum pname, const GLfixed *param);
   ; GL_API void GL_APIENTRY glLightx (GLenum light, GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glLightxv (GLenum light, GLenum pname, const GLfixed *params);
   ; GL_API void GL_APIENTRY glLineWidthx (GLfixed width);
   (define glLoadIdentity (GLES GLvoid "glLoadIdentity"))
   ; GL_API void GL_APIENTRY glLoadMatrixx (const GLfixed *m);
   ; GL_API void GL_APIENTRY glLogicOp (GLenum opcode);
   ; GL_API void GL_APIENTRY glMaterialx (GLenum face, GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glMaterialxv (GLenum face, GLenum pname, const GLfixed *param);
   (define glMatrixMode (GLES GLvoid "glMatrixMode" GLenum))
   ; GL_API void GL_APIENTRY glMultMatrixx (const GLfixed *m);
   ; GL_API void GL_APIENTRY glMultiTexCoord4x (GLenum texture, GLfixed s, GLfixed t, GLfixed r, GLfixed q);
   ; GL_API void GL_APIENTRY glNormal3x (GLfixed nx, GLfixed ny, GLfixed nz);
   ; GL_API void GL_APIENTRY glNormalPointer (GLenum type, GLsizei stride, const void *pointer);
   ; GL_API void GL_APIENTRY glOrthox (GLfixed l, GLfixed r, GLfixed b, GLfixed t, GLfixed n, GLfixed f);
   (define glPixelStorei (GLES GLvoid "glPixelStorei" GLenum GLint))
   ; GL_API void GL_APIENTRY glPointParameterx (GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glPointParameterxv (GLenum pname, const GLfixed *params);
   ; GL_API void GL_APIENTRY glPointSizex (GLfixed size);
   ; GL_API void GL_APIENTRY glPolygonOffsetx (GLfixed factor, GLfixed units);
   (define glPopMatrix (GLES GLvoid "glPopMatrix"))
   (define glPushMatrix (GLES GLvoid "glPushMatrix"))
   ; GL_API void GL_APIENTRY glReadPixels (GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type, void *pixels);
   ; GL_API void GL_APIENTRY glRotatex (GLfixed angle, GLfixed x, GLfixed y, GLfixed z);
   ; GL_API void GL_APIENTRY glSampleCoverage (GLfloat value, GLboolean invert);
   ; GL_API void GL_APIENTRY glSampleCoveragex (GLclampx value, GLboolean invert);
   ; GL_API void GL_APIENTRY glScalex (GLfixed x, GLfixed y, GLfixed z);
   ; GL_API void GL_APIENTRY glScissor (GLint x, GLint y, GLsizei width, GLsizei height);
   (define glShadeModel (GLES GLvoid "glShadeModel" GLenum))
   ; GL_API void GL_APIENTRY glStencilFunc (GLenum func, GLint ref, GLuint mask);
   ; GL_API void GL_APIENTRY glStencilMask (GLuint mask);
   ; GL_API void GL_APIENTRY glStencilOp (GLenum fail, GLenum zfail, GLenum zpass);
   (define glTexCoordPointer (GLES GLvoid "glTexCoordPointer" GLint GLenum GLsizei fft-any))
   ; GL_API void GL_APIENTRY glTexEnvi (GLenum target, GLenum pname, GLint param);
   ; GL_API void GL_APIENTRY glTexEnvx (GLenum target, GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glTexEnviv (GLenum target, GLenum pname, const GLint *params);
   ; GL_API void GL_APIENTRY glTexEnvxv (GLenum target, GLenum pname, const GLfixed *params);
   (define glTexImage2D (GLES GLvoid "glTexImage2D" GLenum GLint GLint GLsizei GLsizei GLint GLenum GLenum fft-any))
   (define glTexParameteri (GLES GLvoid "glTexParameteri" GLenum GLenum GLint))
   ; GL_API void GL_APIENTRY glTexParameterx (GLenum target, GLenum pname, GLfixed param);
   ; GL_API void GL_APIENTRY glTexParameteriv (GLenum target, GLenum pname, const GLint *params);
   ; GL_API void GL_APIENTRY glTexParameterxv (GLenum target, GLenum pname, const GLfixed *params);
   (define glTexSubImage2D (GLES GLvoid "glTexSubImage2D" GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum fft-any))
   ; GL_API void GL_APIENTRY glTranslatex (GLfixed x, GLfixed y, GLfixed z);

   (define glVertexPointer (GLES GLvoid "glVertexPointer" GLint GLenum GLsizei fft-any))
   ;; (define (glVertexPointer size type stride pointer)
   ;;    (case type
   ;;       (GL_FLOAT
   ;;          (glVertexPointerf size type stride pointer))
   ;;       (else
   ;;          (runtime-error "oops" type))))

   (define glViewport (GLES GLvoid "glViewport" GLint GLint GLsizei GLsizei))

   ; ========================================================================
   ; additional glBegin/glEnd compatibility functions

   (define fft-float* (fft* fft-float))

   (actor 'opengl-compat (lambda ()
      ; some internal staff
      (define default-color '(1 1 1 1))

   ; main loop
   (let this ((dictionary #empty))
   (let* ((envelope (wait-mail))
         (sender msg envelope))
      (case msg
         (['debug]
            (mail sender dictionary)
            (this dictionary))
         
         ; math
         (['glOrtho l r b t n f]
            (let*((dictionary (put dictionary 'ortho [l r b t n f])))
               (this dictionary)))

         ; drawing
         (['glBegin mode]
            (let*((dictionary (put dictionary 'mode mode))
                  (dictionary (put dictionary 'vertices #null))
                  (dictionary (put dictionary 'colors #null))
                  (dictionary (put dictionary 'texcoords #null))
                  (dictionary (put dictionary 'vbos ((lambda ()
                                 (define vbos '(0 0 0))
                                 (glGenBuffers 3 vbos)
                                 vbos)))))
               (this dictionary)))
         (['glColor r g b a]
            (let ((dictionary (put dictionary 'color (list r g b a))))
               (this dictionary)))
         (['glVertex x y z]
            (let*((ortho (get dictionary 'ortho [0 1 0 1 0 1]))
                  (dictionary (put dictionary 'vertices (append
                                 (get dictionary 'vertices '())
                                 (list (- (* x 2) 1)
                                       (- (* y 2) 1)
                                       z))))
                  (dictionary (put dictionary 'colors (append
                                 (get dictionary 'colors '())
                                 (get dictionary 'color default-color)))))
               (this dictionary)))
         (['glTexCoord s t]
            (let*((dictionary (put dictionary 'texcoords (append
                                 (get dictionary 'texcoords '())
                                 (list s t)))))
               (this dictionary)))

         (['glEnd]
            (let ((vbos (get dictionary 'vbos '(0 0)))
                  (mode (get dictionary 'mode GL_TRIANGLES))
                  (vertices (get dictionary 'vertices #null))
                  (texcoords (get dictionary 'texcoords #f))
                  (colors (get dictionary 'colors #null)))

;;                   ;(vPosition (glGetAttribLocation (get dictionary 'program 0) "vPosition"))
;;                   ;(vColor    (glGetAttribLocation (get dictionary 'program 0) "vColor")))
               (glBindBuffer GL_ARRAY_BUFFER (lref vbos 0))
               (glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length vertices)) (cons fft-float* vertices) GL_STATIC_DRAW)
               (glVertexPointer 3 GL_FLOAT 0 #f)
               (glEnableClientState GL_VERTEX_ARRAY)

               (unless (null? colors)
                  (glBindBuffer GL_ARRAY_BUFFER (lref vbos 1))
                  (glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length colors)) (cons fft-float* colors) GL_STATIC_DRAW)
                  (glColorPointer 4 GL_FLOAT 0 #f)
                  (glEnableClientState GL_COLOR_ARRAY)
               )

               (unless (null? texcoords)
                  (glBindBuffer GL_ARRAY_BUFFER (lref vbos 2))
                  (glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length texcoords)) (cons fft-float* texcoords) GL_STATIC_DRAW)
                  (glTexCoordPointer 2 GL_FLOAT 0 #f)
                  (glEnableClientState GL_TEXTURE_COORD_ARRAY)
               )

               (glDrawArrays mode 0 (/ (length vertices) 3))

               (glDisableClientState GL_TEXTURE_COORD_ARRAY)
               (glDisableClientState GL_COLOR_ARRAY)
               (glDisableClientState GL_VERTEX_ARRAY)



;;                ;; ;(glUseProgram (get dictionary 'program 0))
;;                ;; ; matrices
;;                ;; ;; (glUniformMatrix4fv (glGetUniformLocation (get dictionary 'program 0) "uModelViewMatrix")
;;                ;; ;;    1 GL_FALSE (apply append (get dictionary GL_MODELVIEW_MATRIX identity-matrix)))
;;                ;; ;; (glUniformMatrix4fv (glGetUniformLocation (get dictionary 'program 0) "uProjectionMatrix")
;;                ;; ;;    1 GL_FALSE (apply append (get dictionary GL_PROJECTION_MATRIX identity-matrix)))
;;                ;; ; vertices
;;                ;; (glBindBuffer GL_ARRAY_BUFFER (list-ref vbos 0))
;;                ;; (glVertexAttribPointer vPosition 3 GL_FLOAT GL_FALSE 0 #false)
;;                ;; (glEnableVertexAttribArray vPosition)
;;                ;; ; colors
;;                ;; (glBindBuffer GL_ARRAY_BUFFER (list-ref vbos 1))
;;                ;; (glVertexAttribPointer vColor 4 GL_FLOAT GL_FALSE 0 #false)
;;                ;; (glEnableVertexAttribArray vColor)
;;                ;; (glDrawArrays mode 0 (/ (length vertices) 3))

;;                ; free resources
               (glDeleteBuffers (length vbos) vbos))
            (mail sender 'ok)
            (this dictionary))

         (else
            (print-to stderr "Unknown opengl server command " msg)
            (this dictionary)))))))

   (define (glBegin mode)
      (mail 'opengl-compat ['glBegin mode]))
   (define (glEnd)
      (await (mail 'opengl-compat ['glEnd])))

   (define (glVertex2f x y)
      (mail 'opengl-compat ['glVertex x y 0]))

   (define (glColor3f x y z)
      (mail 'opengl-compat ['glColor x y z 1]))

   (define (glTexCoord2f s t)
      (mail 'opengl-compat ['glTexCoord s t]))

   (define (glOrtho l r b t n f)
      (mail 'opengl-compat ['glOrtho l r b t n f]))

))