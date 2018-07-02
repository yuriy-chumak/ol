;
; EXT_vertex_array
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
(define-library (OpenGL EXT vertex_array)

; --------------------------------------------------------------------------
; Dependencies
;     None
   (import
      (scheme core) (owl io)
      (OpenGL version-1-0))

; --------------------------------------------------------------------------
   (export  EXT_vertex_array

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
;   (gl:make-current)
   (define EXT_vertex_array (gl:ExtensionSupported? "GL_EXT_vertex_array"))

		(define GL_VERTEX_ARRAY #x8074)
		(define GL_NORMAL_ARRAY #x8075)
		(define GL_COLOR_ARRAY #x8076)
		(define GL_INDEX_ARRAY #x8077)
		(define GL_TEXTURE_COORD_ARRAY #x8078)
		(define GL_EDGE_FLAG_ARRAY #x8079)
		(define GL_VERTEX_ARRAY_SIZE #x807A)
		(define GL_VERTEX_ARRAY_TYPE #x807B)
		(define GL_VERTEX_ARRAY_STRIDE #x807C)
		(define GL_NORMAL_ARRAY_TYPE #x807E)
		(define GL_NORMAL_ARRAY_STRIDE #x807F)
		(define GL_COLOR_ARRAY_SIZE #x8081)
		(define GL_COLOR_ARRAY_TYPE #x8082)
		(define GL_COLOR_ARRAY_STRIDE #x8083)
		(define GL_INDEX_ARRAY_TYPE #x8085)
		(define GL_INDEX_ARRAY_STRIDE #x8086)
		(define GL_TEXTURE_COORD_ARRAY_SIZE #x8088)
		(define GL_TEXTURE_COORD_ARRAY_TYPE #x8089)
		(define GL_TEXTURE_COORD_ARRAY_STRIDE #x808A)
		(define GL_EDGE_FLAG_ARRAY_STRIDE #x808C)
		(define GL_VERTEX_ARRAY_POINTER #x808E)
		(define GL_NORMAL_ARRAY_POINTER #x808F)
		(define GL_COLOR_ARRAY_POINTER #x8090)
		(define GL_INDEX_ARRAY_POINTER #x8091)
		(define GL_TEXTURE_COORD_ARRAY_POINTER #x8092)
		(define GL_EDGE_FLAG_ARRAY_POINTER #x8093)
		(define GL_V2F #x2A20)
		(define GL_V3F #x2A21)
		(define GL_C4UB_V2F #x2A22)
		(define GL_C4UB_V3F #x2A23)
		(define GL_C3F_V3F #x2A24)
		(define GL_N3F_V3F #x2A25)
		(define GL_C4F_N3F_V3F #x2A26)
		(define GL_T2F_V3F #x2A27)
		(define GL_T4F_V4F #x2A28)
		(define GL_T2F_C4UB_V3F #x2A29)
		(define GL_T2F_C3F_V3F #x2A2A)
		(define GL_T2F_N3F_V3F #x2A2B)
		(define GL_T2F_C4F_N3F_V3F #x2A2C)
		(define GL_T4F_C4F_N3F_V4F #x2A2D)

;   (gl:stop-current)
))
