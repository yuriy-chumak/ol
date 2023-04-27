; ===========================================================================
; EXT_geometry_shader
;
;    A new shader type available to be run on the GPU, called a geometry shader.
;
;    https://registry.khronos.org/OpenGL/extensions/EXT/EXT_geometry_shader.txt
;
; Version
;    Last Modified Date: May 31, 2016
;    Revision: 21
;
; Number
;    OpenGL ES Extension #177
;
; Overview
;    EXT_geometry_shader defines a new shader type available to be run on the
;    GPU, called a geometry shader. Geometry shaders are run after vertices are
;    transformed, but prior to color clamping, flatshading and clipping.
(define-library (OpenGL EXT geometry_shader)

; ---------------------------------------------------------------------------
; Dependencies
;    OpenGL ES 3.1 and OpenGL ES Shading Language 3.10 are required.

(import (scheme core)
        (OpenGL platform))

; ---------------------------------------------------------------------------
(export EXT_geometry_shader

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glFramebufferTexture

; ---------------------------------------------------------------------------
; New Tokens

   GL_GEOMETRY_SHADER
   GL_GEOMETRY_SHADER_BIT

   GL_GEOMETRY_LINKED_VERTICES_OUT
   GL_GEOMETRY_LINKED_INPUT_TYPE
   GL_GEOMETRY_LINKED_OUTPUT_TYPE
   GL_GEOMETRY_SHADER_INVOCATIONS

   GL_LAYER_PROVOKING_VERTEX
   GL_MAX_GEOMETRY_UNIFORM_COMPONENTS
   GL_MAX_GEOMETRY_UNIFORM_BLOCKS
   GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS
   GL_MAX_GEOMETRY_INPUT_COMPONENTS
   GL_MAX_GEOMETRY_OUTPUT_COMPONENTS
   GL_MAX_GEOMETRY_OUTPUT_VERTICES
   GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS
   GL_MAX_GEOMETRY_SHADER_INVOCATIONS
   GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS
   GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS
   GL_MAX_GEOMETRY_ATOMIC_COUNTERS
   GL_MAX_GEOMETRY_IMAGE_UNIFORMS
   GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS

   GL_FIRST_VERTEX_CONVENTION
   GL_LAST_VERTEX_CONVENTION
   GL_UNDEFINED_VERTEX

   GL_PRIMITIVES_GENERATED

   GL_LINES_ADJACENCY
   GL_LINE_STRIP_ADJACENCY
   GL_TRIANGLES_ADJACENCY
   GL_TRIANGLE_STRIP_ADJACENCY

   GL_FRAMEBUFFER_DEFAULT_LAYERS
   GL_MAX_FRAMEBUFFER_LAYERS
   GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS
   GL_FRAMEBUFFER_ATTACHMENT_LAYERED
   GL_REFERENCED_BY_GEOMETRY_SHADER
)

; ---------------------------------------------------------------------------
(begin
   (define EXT_geometry_shader (gl:QueryExtension "GL_EXT_geometry_shader"))
   (setq GL gl:GetProcAddress)
   
   (define glFramebufferTexture (GL GLvoid "glFramebufferTextureEXT" GLenum GLenum GLuint GLint))

   (define GL_GEOMETRY_SHADER                             #x8DD9)
   (define GL_GEOMETRY_SHADER_BIT                         #x00000004)

   (define GL_GEOMETRY_LINKED_VERTICES_OUT                #x8916)
   (define GL_GEOMETRY_LINKED_INPUT_TYPE                  #x8917)
   (define GL_GEOMETRY_LINKED_OUTPUT_TYPE                 #x8918)
   (define GL_GEOMETRY_SHADER_INVOCATIONS                 #x887F)

   (define GL_LAYER_PROVOKING_VERTEX                      #x825E)
   (define GL_MAX_GEOMETRY_UNIFORM_COMPONENTS             #x8DDF)
   (define GL_MAX_GEOMETRY_UNIFORM_BLOCKS                 #x8A2C)
   (define GL_MAX_COMBINED_GEOMETRY_UNIFORM_COMPONENTS    #x8A32)
   (define GL_MAX_GEOMETRY_INPUT_COMPONENTS               #x9123)
   (define GL_MAX_GEOMETRY_OUTPUT_COMPONENTS              #x9124)
   (define GL_MAX_GEOMETRY_OUTPUT_VERTICES                #x8DE0)
   (define GL_MAX_GEOMETRY_TOTAL_OUTPUT_COMPONENTS        #x8DE1)
   (define GL_MAX_GEOMETRY_SHADER_INVOCATIONS             #x8E5A)
   (define GL_MAX_GEOMETRY_TEXTURE_IMAGE_UNITS            #x8C29)
   (define GL_MAX_GEOMETRY_ATOMIC_COUNTER_BUFFERS         #x92CF)
   (define GL_MAX_GEOMETRY_ATOMIC_COUNTERS                #x92D5)
   (define GL_MAX_GEOMETRY_IMAGE_UNIFORMS                 #x90CD)
   (define GL_MAX_GEOMETRY_SHADER_STORAGE_BLOCKS          #x90D7)

   (define GL_FIRST_VERTEX_CONVENTION                     #x8E4D)
   (define GL_LAST_VERTEX_CONVENTION                      #x8E4E)
   (define GL_UNDEFINED_VERTEX                            #x8260)

   (define GL_PRIMITIVES_GENERATED                        #x8C87)

   (define GL_LINES_ADJACENCY                             #xA)
   (define GL_LINE_STRIP_ADJACENCY                        #xB)
   (define GL_TRIANGLES_ADJACENCY                         #xC)
   (define GL_TRIANGLE_STRIP_ADJACENCY                    #xD)

   (define GL_FRAMEBUFFER_DEFAULT_LAYERS                  #x9312)
   (define GL_MAX_FRAMEBUFFER_LAYERS                      #x9317)
   (define GL_FRAMEBUFFER_INCOMPLETE_LAYER_TARGETS        #x8DA8)
   (define GL_FRAMEBUFFER_ATTACHMENT_LAYERED              #x8DA7)
   (define GL_REFERENCED_BY_GEOMETRY_SHADER               #x9309)
))
