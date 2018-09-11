; ==========================================================================
; ARB_shader_objects
;     Multiple vertices may be passed to the GL with a single function call.
;
;     https://www.opengl.org/registry/specs/EXT/vertex_array.txt
;
; Version
;     $Date: 1995/10/03 05:39:58 $ $Revision: 1.16 $  FINAL
;
; Overview
;     This extension adds the ability to specify multiple geometric primitives
;     with very few subroutine calls.  Instead of calling an OpenGL procedure
;     to pass each individual vertex, normal, or color, separate arrays
;     of vertexes, normals, and colors are prespecified, and are used to
;     define a sequence of primitives (all of the same type) when a single
;     call is made to DrawArraysEXT.  A stride mechanism is provided so that
;     an application can choose to keep all vertex data staggered in a
;     single array, or sparsely in separate arrays.  Single-array storage
;     may optimize performance on some implementations.
;
;     This extension also supports the rendering of individual array elements,
;     each specified as an index into the enabled arrays.
(define-library (OpenGL ARB shader_object)

; --------------------------------------------------------------------------
; Dependencies
;     None
   (import
      (scheme core) (owl io)
      (OpenGL version-1-0))

; --------------------------------------------------------------------------
   (export  ARB_shader_object

; --------------------------------------------------------------------------
; New Procedures and Functions
   ;ArrayElementEXT
   ;DrawArraysEXT
   ;VertexPointerEXT
   ;NormalPointerEXT
   ;ColorPointerEXT
   ;IndexPointerEXT
   ;TexCoordPointerEXT
   ;EdgeFlagPointerEXT
   ;GetPointervEXT

; --------------------------------------------------------------------------
; New Tokens
;
;	Accepted by the <cap> parameter of Enable, Disable, and IsEnabled, and
;	by the <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and
;	GetDoublev:
;        VERTEX_ARRAY_EXT               0x8074
;        NORMAL_ARRAY_EXT               0x8075
;        COLOR_ARRAY_EXT                0x8076
;        INDEX_ARRAY_EXT                0x8077
;        TEXTURE_COORD_ARRAY_EXT        0x8078
;        EDGE_FLAG_ARRAY_EXT            0x8079

;	Accepted by the <pname> parameter of GetPointervEXT:
;        VERTEX_ARRAY_POINTER_EXT       0x808E
;        NORMAL_ARRAY_POINTER_EXT       0x808F
;        COLOR_ARRAY_POINTER_EXT        0x8090
;        INDEX_ARRAY_POINTER_EXT        0x8091
;        TEXTURE_COORD_ARRAY_POINTER_EXT 0x8092
;        EDGE_FLAG_ARRAY_POINTER_EXT    0x8093


)

; --------------------------------------------------------------------------
   (begin
   (define ARB_shader_object (gl:ExtensionSupported? "GL_ARB_shader_object"))

))
