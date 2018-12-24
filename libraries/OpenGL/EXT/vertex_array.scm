; ===========================================================================
; EXT_vertex_array                                   (included in OpenGL 1.1)
;
;     Multiple vertices may be passed to the GL with a single function call.
;
;     https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_vertex_array.txt
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
(define-library (OpenGL EXT vertex_array)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import (scheme core) (OpenGL platform))

; --------------------------------------------------------------------------
(export EXT_vertex_array

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

;	Accepted by the <type> parameter of VertexPointerEXT, NormalPointerEXT,
;	ColorPointerEXT, IndexPointerEXT, and TexCoordPointerEXT:
;        DOUBLE_EXT                     0x140A

;	Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;	GetFloatv, and GetDoublev:
;        VERTEX_ARRAY_SIZE_EXT          0x807A
;        VERTEX_ARRAY_TYPE_EXT          0x807B
;        VERTEX_ARRAY_STRIDE_EXT        0x807C
;        VERTEX_ARRAY_COUNT_EXT         0x807D
;        NORMAL_ARRAY_TYPE_EXT          0x807E
;        NORMAL_ARRAY_STRIDE_EXT        0x807F
;        NORMAL_ARRAY_COUNT_EXT         0x8080
;        COLOR_ARRAY_SIZE_EXT           0x8081
;        COLOR_ARRAY_TYPE_EXT           0x8082
;        COLOR_ARRAY_STRIDE_EXT         0x8083
;        COLOR_ARRAY_COUNT_EXT          0x8084
;        INDEX_ARRAY_TYPE_EXT           0x8085
;        INDEX_ARRAY_STRIDE_EXT         0x8086
;        INDEX_ARRAY_COUNT_EXT          0x8087
;        TEXTURE_COORD_ARRAY_SIZE_EXT   0x8088
;        TEXTURE_COORD_ARRAY_TYPE_EXT   0x8089
;        TEXTURE_COORD_ARRAY_STRIDE_EXT 0x808A
;        TEXTURE_COORD_ARRAY_COUNT_EXT  0x808B
;        EDGE_FLAG_ARRAY_STRIDE_EXT     0x808C
;        EDGE_FLAG_ARRAY_COUNT_EXT      0x808D

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
   (define EXT_vertex_array (gl:QueryExtension "GL_EXT_vertex_array"))

))
