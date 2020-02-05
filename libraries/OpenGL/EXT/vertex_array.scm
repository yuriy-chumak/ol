; ===========================================================================
; EXT_vertex_array                                   (included in OpenGL 1.1)
;
;    Multiple vertices may be passed to the GL with a single function call.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_vertex_array.txt
;
; Version
;    $Date: 1995/10/03 05:39:58 $ $Revision: 1.16 $  FINAL
;
; Overview
;    This extension adds the ability to specify multiple geometric primitives
;    with very few subroutine calls.  Instead of calling an OpenGL procedure
;    to pass each individual vertex, normal, or color, separate arrays
;    of vertexes, normals, and colors are prespecified, and are used to
;    define a sequence of primitives (all of the same type) when a single
;    call is made to DrawArraysEXT.  A stride mechanism is provided so that
;    an application can choose to keep all vertex data staggered in a
;    single array, or sparsely in separate arrays.  Single-array storage
;    may optimize performance on some implementations.
;
;    This extension also supports the rendering of individual array elements,
;    each specified as an index into the enabled arrays.
(define-library (OpenGL EXT vertex_array)

; --------------------------------------------------------------------------
; Dependencies
;    None
(import (scheme core)
        (OpenGL platform))

; --------------------------------------------------------------------------
(export EXT_vertex_array

; --------------------------------------------------------------------------
; New Procedures and Functions

   glArrayElementEXT
   glDrawArraysEXT
   glVertexPointerEXT
   glNormalPointerEXT
   glColorPointerEXT
   glIndexPointerEXT
   glTexCoordPointerEXT
   glEdgeFlagPointerEXT
   glGetPointervEXT

; --------------------------------------------------------------------------
; New Tokens
;
;    Accepted by the <cap> parameter of Enable, Disable, and IsEnabled, and
;    by the <pname> parameter of GetBooleanv, GetIntegerv, GetFloatv, and
;    GetDoublev:

   GL_VERTEX_ARRAY_EXT
   GL_NORMAL_ARRAY_EXT
   GL_COLOR_ARRAY_EXT
   GL_INDEX_ARRAY_EXT
   GL_TEXTURE_COORD_ARRAY_EXT
   GL_EDGE_FLAG_ARRAY_EXT

;    Accepted by the <type> parameter of VertexPointerEXT, NormalPointerEXT,
;    ColorPointerEXT, IndexPointerEXT, and TexCoordPointerEXT:

   GL_DOUBLE_EXT

;    Accepted by the <pname> parameter of GetBooleanv, GetIntegerv,
;    GetFloatv, and GetDoublev:

   GL_VERTEX_ARRAY_SIZE_EXT
   GL_VERTEX_ARRAY_TYPE_EXT
   GL_VERTEX_ARRAY_STRIDE_EXT
   GL_VERTEX_ARRAY_COUNT_EXT
   GL_NORMAL_ARRAY_TYPE_EXT
   GL_NORMAL_ARRAY_STRIDE_EXT
   GL_NORMAL_ARRAY_COUNT_EXT
   GL_COLOR_ARRAY_SIZE_EXT
   GL_COLOR_ARRAY_TYPE_EXT
   GL_COLOR_ARRAY_STRIDE_EXT
   GL_COLOR_ARRAY_COUNT_EXT
   GL_INDEX_ARRAY_TYPE_EXT
   GL_INDEX_ARRAY_STRIDE_EXT
   GL_INDEX_ARRAY_COUNT_EXT
   GL_TEXTURE_COORD_ARRAY_SIZE_EXT
   GL_TEXTURE_COORD_ARRAY_TYPE_EXT
   GL_TEXTURE_COORD_ARRAY_STRIDE_EXT
   GL_TEXTURE_COORD_ARRAY_COUNT_EXT
   GL_EDGE_FLAG_ARRAY_STRIDE_EXT
   GL_EDGE_FLAG_ARRAY_COUNT_EXT

;    Accepted by the <pname> parameter of GetPointervEXT:

   GL_VERTEX_ARRAY_POINTER_EXT
   GL_NORMAL_ARRAY_POINTER_EXT
   GL_COLOR_ARRAY_POINTER_EXT
   GL_INDEX_ARRAY_POINTER_EXT
   GL_TEXTURE_COORD_ARRAY_POINTER_EXT
   GL_EDGE_FLAG_ARRAY_POINTER_EXT

)

; --------------------------------------------------------------------------
(begin
   (define EXT_vertex_array (gl:QueryExtension "GL_EXT_vertex_array"))

   (setq GL GL_LIBRARY)
   (define glArrayElementEXT (GL GLvoid "glArrayElementEXT" GLint))
   (define glDrawArraysEXT (GL GLvoid "glDrawArraysEXT" GLenum GLint GLsizei))
   (define glVertexPointerEXT (GL GLvoid "glVertexPointerEXT" GLint GLenum GLsizei GLsizei fft-any))
   (define glNormalPointerEXT (GL GLvoid "glNormalPointerEXT" GLenum GLsizei GLsizei fft-any))
   (define glColorPointerEXT (GL GLvoid "glColorPointerEXT" GLint GLenum GLsizei GLsizei fft-any))
   (define glIndexPointerEXT (GL GLvoid "glIndexPointerEXT" GLenum GLsizei GLsizei fft-any))
   (define glTexCoordPointerEXT (GL GLvoid "glTexCoordPointerEXT" GLint GLenum GLsizei GLsizei fft-any))
   (define glEdgeFlagPointerEXT (GL GLvoid "glEdgeFlagPointerEXT" GLsizei GLsizei GLboolean*))
   (define glGetPointervEXT (GL GLvoid "glGetPointervEXT" GLenum (fft& type-vptr)))

   (define GL_VERTEX_ARRAY_EXT               #x8074)
   (define GL_NORMAL_ARRAY_EXT               #x8075)
   (define GL_COLOR_ARRAY_EXT                #x8076)
   (define GL_INDEX_ARRAY_EXT                #x8077)
   (define GL_TEXTURE_COORD_ARRAY_EXT        #x8078)
   (define GL_EDGE_FLAG_ARRAY_EXT            #x8079)
   (define GL_DOUBLE_EXT                     #x140A)
   (define GL_VERTEX_ARRAY_SIZE_EXT          #x807A)
   (define GL_VERTEX_ARRAY_TYPE_EXT          #x807B)
   (define GL_VERTEX_ARRAY_STRIDE_EXT        #x807C)
   (define GL_VERTEX_ARRAY_COUNT_EXT         #x807D)
   (define GL_NORMAL_ARRAY_TYPE_EXT          #x807E)
   (define GL_NORMAL_ARRAY_STRIDE_EXT        #x807F)
   (define GL_NORMAL_ARRAY_COUNT_EXT         #x8080)
   (define GL_COLOR_ARRAY_SIZE_EXT           #x8081)
   (define GL_COLOR_ARRAY_TYPE_EXT           #x8082)
   (define GL_COLOR_ARRAY_STRIDE_EXT         #x8083)
   (define GL_COLOR_ARRAY_COUNT_EXT          #x8084)
   (define GL_INDEX_ARRAY_TYPE_EXT           #x8085)
   (define GL_INDEX_ARRAY_STRIDE_EXT         #x8086)
   (define GL_INDEX_ARRAY_COUNT_EXT          #x8087)
   (define GL_TEXTURE_COORD_ARRAY_SIZE_EXT   #x8088)
   (define GL_TEXTURE_COORD_ARRAY_TYPE_EXT   #x8089)
   (define GL_TEXTURE_COORD_ARRAY_STRIDE_EXT #x808A)
   (define GL_TEXTURE_COORD_ARRAY_COUNT_EXT  #x808B)
   (define GL_EDGE_FLAG_ARRAY_STRIDE_EXT     #x808C)
   (define GL_EDGE_FLAG_ARRAY_COUNT_EXT      #x808D)
   (define GL_VERTEX_ARRAY_POINTER_EXT       #x808E)
   (define GL_NORMAL_ARRAY_POINTER_EXT       #x808F)
   (define GL_COLOR_ARRAY_POINTER_EXT        #x8090)
   (define GL_INDEX_ARRAY_POINTER_EXT        #x8091)
   (define GL_TEXTURE_COORD_ARRAY_POINTER_EXT #x8092)
   (define GL_EDGE_FLAG_ARRAY_POINTER_EXT    #x8093)

))
