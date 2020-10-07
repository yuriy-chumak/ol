; ===========================================================================
; EXT_geometry_shader4                               (included in OpenGL 3.0)
;
;    A new shader type available to be run on the GPU, called a geometry shader.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_geometry_shader4.txt
;
; Version
;    Last Modified Date:         12/14/2009
;    NVIDIA Revision:            22
;
; Number
;    324
;
; Overview
;    This extension defines methods to load texture images directly from the
;    framebuffer.  Methods are defined for both complete and partial
;    replacement of a texture image.  Because it is not possible to define
;    an entire 3D texture using a 2D framebuffer image, 3D textures are
;    supported only for partial replacement.
(define-library (OpenGL EXT geometry_shader4)

; ---------------------------------------------------------------------------
; Dependencies
;    This extension is written against the OpenGL 2.0 specification.
;    EXT_framebuffer_object interacts with this extension.
;    EXT_framebuffer_blit interacts with this extension.
;    EXT_texture_array interacts with this extension.
;    ARB_texture_rectangle trivially affects the definition of this extension.
;    EXT_texture_buffer_object trivially affects the definition of this extension.
;    NV_primitive_restart trivially affects the definition of this extension.
;    This extension interacts with EXT_tranform_feedback.
(import (scheme core)
        (OpenGL platform))

;    OpenGL 1.1 is required.
(import (OpenGL version-1-1))

; ---------------------------------------------------------------------------
(export EXT_geometry_shader4

; ---------------------------------------------------------------------------
; New Procedures and Functions

   ;; glProgramParameteriEXT ;(uint program, enum pname, int value);
   ;; glFramebufferTextureEXT ;(enum target, enum attachment, uint texture, int level);
   ;; glFramebufferTextureLayerEXT ;(enum target, enum attachment, uint texture, int level, int layer);
   ;; glFramebufferTextureFaceEXT ;(enum target, enum attachment, uint texture, int level, enum face);

; ---------------------------------------------------------------------------
; New Tokens

;    Accepted by the <type> parameter of CreateShader and returned by the
;    <params> parameter of GetShaderiv:

        GL_GEOMETRY_SHADER

;    Accepted by the <pname> parameter of ProgramParameteriEXT and
;    GetProgramiv:

        GL_GEOMETRY_VERTICES_OUT
        GL_GEOMETRY_INPUT_TYPE
        GL_GEOMETRY_OUTPUT_TYPE

;    Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;    GetFloatv, and GetDoublev:

        GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
        GL_MAX_GEOMETRY_VARYING_COMPONENTS
        GL_MAX_VERTEX_VARYING_COMPONENTS
        GL_MAX_VARYING_COMPONENTS
        GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
        GL_MAX_GEOMETRY_OUTPUT_VERTICES
        GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS

;    Accepted by the <mode> parameter of Begin, DrawArrays,
;    MultiDrawArrays, DrawElements, MultiDrawElements, and
;    DrawRangeElements:

        GL_LINES_ADJACENCY
        GL_LINE_STRIP_ADJACENCY
        GL_TRIANGLES_ADJACENCY
        GL_TRIANGLE_STRIP_ADJACENCY

;    Returned by CheckFramebufferStatusEXT:

        GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
        GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT

;    Accepted by the <pname> parameter of GetFramebufferAttachment-
;    ParameterivEXT:

        GL_FRAMEBUFFER_ATTACHMENT_LAYERED
        GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER

;    Accepted by the <cap> parameter of Enable, Disable, and IsEnabled,
;    and by the <pname> parameter of GetIntegerv, GetFloatv, GetDoublev,
;    and GetBooleanv:

        GL_PROGRAM_POINT_SIZE

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_geometry_shader4 (gl:QueryExtension "GL_EXT_geometry_shader4"))

   (setq GL GL_LIBRARY)
   ;;  void ProgramParameteriEXT(uint program, enum pname, int value);
   ;;  void FramebufferTextureEXT(enum target, enum attachment, 
   ;;                             uint texture, int level);
   ;;  void FramebufferTextureLayerEXT(enum target, enum attachment, 
   ;;                                  uint texture, int level, int layer);
   ;;  void FramebufferTextureFaceEXT(enum target, enum attachment,
   ;;                                 uint texture, int level, enum face);

        (define GL_GEOMETRY_SHADER                              #x8DD9)

        (define GL_GEOMETRY_VERTICES_OUT                        #x8DDA)
        (define GL_GEOMETRY_INPUT_TYPE                          #x8DDB)
        (define GL_GEOMETRY_OUTPUT_TYPE                         #x8DDC)

        (define GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS             #x8C29)
        (define GL_MAX_GEOMETRY_VARYING_COMPONENTS              #x8DDD)
        (define GL_MAX_VERTEX_VARYING_COMPONENTS                #x8DDE)
        (define GL_MAX_VARYING_COMPONENTS                       #x8B4B)
        (define GL_MAX_GEOMETRY_UNIFORM_COMPONENTS              #x8DDF)
        (define GL_MAX_GEOMETRY_OUTPUT_VERTICES                 #x8DE0)
        (define GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS         #x8DE1)

        (define GL_LINES_ADJACENCY                              #xA)
        (define GL_LINE_STRIP_ADJACENCY                         #xB)
        (define GL_TRIANGLES_ADJACENCY                          #xC)
        (define GL_TRIANGLE_STRIP_ADJACENCY                     #xD)

        (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS         #x8DA8)
        (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_COUNT           #x8DA9)

        (define GL_FRAMEBUFFER_ATTACHMENT_LAYERED               #x8DA7)
        (define GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER         #x8CD4)

        (define GL_PROGRAM_POINT_SIZE                           #x8642)
))
