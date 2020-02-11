; OpenGL 1.1 (4 Mar 1997)
; в комментариях указаны расширения, которые были интегрированы в этот выпуск
; ===========================================================================
(define-library (OpenGL version-1-1)
(export

   GL_VERSION_1_1

   GL_INDEX_LOGIC_OP          ; renamed from GL_LOGIC_OP by 1.1
   GL_TEXTURE_INTERNAL_FORMAT ; renamed from GL_TEXTURE_COMPONENTS by 1.1

   ; WINGDIAPI void APIENTRY glIndexub (GLubyte c);
   ; WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);

   glEnableClientState ; void (GLenum array)
   glDisableClientState ; void (GLenum array)
   ; InterleavedArrays

   GL_V2F
   GL_V3F
   GL_C4UB_V2F
   GL_C4UB_V3F
   GL_C3F_V3F
   GL_N3F_V3F
   GL_C4F_N3F_V3F
   GL_T2F_V3F
   GL_T4F_V4F
   GL_T2F_C4UB_V3F
   GL_T2F_C3F_V3F
   GL_T2F_N3F_V3F
   GL_T2F_C4F_N3F_V3F
   GL_T4F_C4F_N3F_V4F

   ; WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
   ; WINGDIAPI void APIENTRY glPopClientAttrib (void);
   GL_CLIENT_PIXEL_STORE_BIT
   GL_CLIENT_VERTEX_ARRAY_BIT
   GL_CLIENT_ALL_ATTRIB_BITS

   GL_CLIENT_ATTRIB_STACK_DEPTH
   GL_MAX_CLIENT_ATTRIB_STACK_DEPTH

   ; DrawArrays
   glDrawElements ; void (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);

 ; C.1 EXT_vertex_array, except that static array data
   ; are not supported (because they complicated the interface, and were not
   ; being used), and the pre-dened congurations are added (both to reduce
   ; subroutine count even further, and to allow for ecient transfer of array
   ; data)
   GL_DOUBLE

   GL_VERTEX_ARRAY
   GL_NORMAL_ARRAY
   GL_COLOR_ARRAY
   GL_INDEX_ARRAY
   GL_TEXTURE_COORD_ARRAY
   GL_EDGE_FLAG_ARRAY

   GL_VERTEX_ARRAY_SIZE
   GL_VERTEX_ARRAY_TYPE
   GL_VERTEX_ARRAY_STRIDE
;  VERTEX_ARRAY_COUNT_EXT  excluded by 1.1
   GL_NORMAL_ARRAY_TYPE
   GL_NORMAL_ARRAY_STRIDE
;  NORMAL_ARRAY_COUNT_EXT  excluded by 1.1
   GL_COLOR_ARRAY_SIZE
   GL_COLOR_ARRAY_TYPE
   GL_COLOR_ARRAY_STRIDE
;  COLOR_ARRAY_COUNT_EXT  excluded by 1.1
   GL_INDEX_ARRAY_TYPE
   GL_INDEX_ARRAY_STRIDE
;  INDEX_ARRAY_COUNT_EXT  excluded by 1.1
   GL_TEXTURE_COORD_ARRAY_SIZE
   GL_TEXTURE_COORD_ARRAY_TYPE
   GL_TEXTURE_COORD_ARRAY_STRIDE
;  TEXTURE_COORD_ARRAY_COUNT_EXT  excluded by 1.1
   GL_EDGE_FLAG_ARRAY_STRIDE
;  EDGE_FLAG_ARRAY_COUNT_EXT  excluded by 1.1

   GL_VERTEX_ARRAY_POINTER
   GL_NORMAL_ARRAY_POINTER
   GL_COLOR_ARRAY_POINTER
   GL_INDEX_ARRAY_POINTER
   GL_TEXTURE_COORD_ARRAY_POINTER
   GL_EDGE_FLAG_ARRAY_POINTER

   glArrayElement  ; void (GLint i)
   glVertexPointer ; void (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)
   glNormalPointer ; void (GLenum type, GLsizei stride, const GLvoid *pointer)
   glColorPointer  ; void (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)
   glIndexPointer  ; void (GLenum type, GLsizei stride, const GLvoid *pointer)
   glTexCoordPointer ; void (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer)
   glEdgeFlagPointer ; void (GLsizei stride, const GLvoid *pointer)
   glGetPointerv ; void (GLenum pname, GLvoid **params)
   glDrawArrays ; void (GLenum mode, GLint first, GLsizei count)


 ; C.2 EXT_polygon_offset
   glPolygonOffset ; void ( GLfloat factor, GLfloat units )

   GL_POLYGON_OFFSET_FILL  ; renamed from GL_POLYGON_OFFSET_EXT by 1.1
   GL_POLYGON_OFFSET_FACTOR
;  GL_POLYGON_OFFSET_BIAS  ; excluded by 1.1
   GL_POLYGON_OFFSET_UNITS ; introduced by 1.1
   GL_POLYGON_OFFSET_POINT ; introduced by 1.1
   GL_POLYGON_OFFSET_LINE  ; introduced by 1.1

 ; C.3 EXT_blend_logic_op
   ; none

 ; C.4, C.5, C.6 EXT_texture
   GL_ALPHA4
   GL_ALPHA8
   GL_ALPHA12
   GL_ALPHA16
   GL_LUMINANCE4
   GL_LUMINANCE8
   GL_LUMINANCE12
   GL_LUMINANCE16
   GL_LUMINANCE4_ALPHA4
   GL_LUMINANCE6_ALPHA2
   GL_LUMINANCE8_ALPHA8
   GL_LUMINANCE12_ALPHA4
   GL_LUMINANCE12_ALPHA12
   GL_LUMINANCE16_ALPHA16
   GL_INTENSITY
   GL_INTENSITY4
   GL_INTENSITY8
   GL_INTENSITY12
   GL_INTENSITY16
;  GL_RGB2     ; excluded by 1.1
   GL_R3_G3_B2 ; introduced by 1.1
   GL_RGB4
   GL_RGB5
   GL_RGB8
   GL_RGB10
   GL_RGB12
   GL_RGB16
   GL_RGBA2
   GL_RGBA4
   GL_RGB5_A1
   GL_RGBA8
   GL_RGB10_A2
   GL_RGBA12
   GL_RGBA16

   GL_TEXTURE_RED_SIZE
   GL_TEXTURE_GREEN_SIZE
   GL_TEXTURE_BLUE_SIZE
   GL_TEXTURE_ALPHA_SIZE
   GL_TEXTURE_LUMINANCE_SIZE
   GL_TEXTURE_INTENSITY_SIZE

;  GL_REPLACE  ; excluded by 1.1 as duplicate

   GL_PROXY_TEXTURE_1D
   GL_PROXY_TEXTURE_2D

;  GL_TEXTURE_TOO_LARGE ; excluded by 1.1

 ; C.7 EXT_copy_texture
   glCopyTexImage1D
   glCopyTexImage2D ; ...
   glCopyTexSubImage1D
   glCopyTexSubImage2D
;  glCopyTexSubImage3D ; excluded by 1.1 (btw, introduced back by 1.2)

 ; C.7 EXT_subtexture
   glTexSubImage1D
   glTexSubImage2D
;  glTexSubImage3D     ; excluded by 1.1 (btw, introduced back by 1.2)

 ; C.8 EXT_texture_object
   glGenTextures ; void ( GLsizei n, GLuint *textures )
   glDeleteTextures ; void (GLsizei n, const GLuint *textures)
   glBindTexture ; void (GLenum target, GLuint texture)
   glPrioritizeTextures
   glAreTexturesResident
   glIsTexture

   GL_TEXTURE_PRIORITY
   GL_TEXTURE_RESIDENT
   GL_TEXTURE_BINDING_1D ; renamed from TEXTURE_1D_BINDING_EXT by 1.1
   GL_TEXTURE_BINDING_2D ; renamed from TEXTURE_2D_BINDING_EXT by 1.1
;  TEXTURE_3D_BINDING ; excluded by 1.1

   GLU_VERSION_1_2

   (exports (OpenGL version-1-0)))

; ============================================================================
; == implementation ==========================================================
(import (scheme core)
   (OpenGL version-1-0))

(begin
   (define GL_VERSION_1_1 1)

   (define GL GL_LIBRARY)

   (define GL_INDEX_LOGIC_OP GL_LOGIC_OP)
   (define GL_TEXTURE_INTERNAL_FORMAT GL_TEXTURE_COMPONENTS)


   ;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
   ;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);

   (define glEnableClientState (GL GLvoid "glEnableClientState" GLenum))
   (define glDisableClientState (GL GLvoid "glDisableClientState" GLenum))
   ;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);

   (define GL_V2F                            #x2A20)
   (define GL_V3F                            #x2A21)
   (define GL_C4UB_V2F                       #x2A22)
   (define GL_C4UB_V3F                       #x2A23)
   (define GL_C3F_V3F                        #x2A24)
   (define GL_N3F_V3F                        #x2A25)
   (define GL_C4F_N3F_V3F                    #x2A26)
   (define GL_T2F_V3F                        #x2A27)
   (define GL_T4F_V4F                        #x2A28)
   (define GL_T2F_C4UB_V3F                   #x2A29)
   (define GL_T2F_C3F_V3F                    #x2A2A)
   (define GL_T2F_N3F_V3F                    #x2A2B)
   (define GL_T2F_C4F_N3F_V3F                #x2A2C)
   (define GL_T4F_C4F_N3F_V4F                #x2A2D)

   ; 6 State and State Requests (TBD.)

   ; WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
   ; WINGDIAPI void APIENTRY glPopClientAttrib (void);
   (define GL_CLIENT_PIXEL_STORE_BIT         #x00000001)
   (define GL_CLIENT_VERTEX_ARRAY_BIT        #x00000002)
   (define GL_CLIENT_ALL_ATTRIB_BITS         #xffffffff)

   (define GL_CLIENT_ATTRIB_STACK_DEPTH      #x0BB1)
   (define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  #x0D3B)

   (define glDrawElements (GL GLvoid "glDrawElements" GLenum GLsizei GLenum fft-any))


 ; C.1 EXT_vertex_array
   (define GL_DOUBLE				               #x140A)

   (define GL_VERTEX_ARRAY                   #x8074)
   (define GL_NORMAL_ARRAY                   #x8075)
   (define GL_COLOR_ARRAY                    #x8076)
   (define GL_INDEX_ARRAY                    #x8077)
   (define GL_TEXTURE_COORD_ARRAY            #x8078)
   (define GL_EDGE_FLAG_ARRAY                #x8079)

   (define GL_VERTEX_ARRAY_SIZE              #x807A)
   (define GL_VERTEX_ARRAY_TYPE              #x807B)
   (define GL_VERTEX_ARRAY_STRIDE            #x807C)
   (define GL_NORMAL_ARRAY_TYPE              #x807E)
   (define GL_NORMAL_ARRAY_STRIDE            #x807F)
   (define GL_COLOR_ARRAY_SIZE               #x8081)
   (define GL_COLOR_ARRAY_TYPE               #x8082)
   (define GL_COLOR_ARRAY_STRIDE             #x8083)
   (define GL_INDEX_ARRAY_TYPE               #x8085)
   (define GL_INDEX_ARRAY_STRIDE             #x8086)
   (define GL_TEXTURE_COORD_ARRAY_SIZE       #x8088)
   (define GL_TEXTURE_COORD_ARRAY_TYPE       #x8089)
   (define GL_TEXTURE_COORD_ARRAY_STRIDE     #x808A)
   (define GL_EDGE_FLAG_ARRAY_STRIDE         #x808C)

   (define GL_VERTEX_ARRAY_POINTER           #x808E)
   (define GL_NORMAL_ARRAY_POINTER           #x808F)
   (define GL_COLOR_ARRAY_POINTER            #x8090)
   (define GL_INDEX_ARRAY_POINTER            #x8091)
   (define GL_TEXTURE_COORD_ARRAY_POINTER    #x8092)
   (define GL_EDGE_FLAG_ARRAY_POINTER        #x8093)

   (define glArrayElement (GL GLvoid "glArrayElement" GLint))
   (define glVertexPointer (GL GLvoid "glVertexPointer" GLint GLenum GLsizei fft-any))
   (define glNormalPointer (GL GLvoid "glNormalPointer" GLenum GLsizei fft-any))
   (define glColorPointer  (GL GLvoid "glColorPointer" GLint GLenum GLsizei fft-any))
   (define glIndexPointer  (GL GLvoid "glIndexPointer" GLenum GLsizei fft-any))
   (define glTexCoordPointer (GL GLvoid "glTexCoordPointer" GLint GLenum GLsizei fft-any))
   (define glEdgeFlagPointer (GL GLvoid "glEdgeFlagPointer" GLsizei fft-any))
   (define glGetPointerv (GL GLvoid "glGetPointerv" GLenum (fft& type-vptr))) ;GLvoid**
   (define glDrawArrays (GL GLvoid "glDrawArrays" GLenum GLint GLsizei))

 ; C.2 EXT_polygon_offset
   (define glPolygonOffset (GL GLvoid "glPolygonOffset" GLfloat GLfloat))

   (define GL_POLYGON_OFFSET_FILL            #x8037)
   (define GL_POLYGON_OFFSET_FACTOR          #x8038)
   (define GL_POLYGON_OFFSET_UNITS           #x2A00)
   (define GL_POLYGON_OFFSET_POINT           #x2A01)
   (define GL_POLYGON_OFFSET_LINE            #x2A02)

 ; C.3 EXT_blend_logic_op
   ; none

 ; C.4, C.5, C.6 EXT_texture
   (define GL_ALPHA4                         #x803B)
   (define GL_ALPHA8                         #x803C)
   (define GL_ALPHA12                        #x803D)
   (define GL_ALPHA16                        #x803E)
   (define GL_LUMINANCE4                     #x803F)
   (define GL_LUMINANCE8                     #x8040)
   (define GL_LUMINANCE12                    #x8041)
   (define GL_LUMINANCE16                    #x8042)
   (define GL_LUMINANCE4_ALPHA4              #x8043)
   (define GL_LUMINANCE6_ALPHA2              #x8044)
   (define GL_LUMINANCE8_ALPHA8              #x8045)
   (define GL_LUMINANCE12_ALPHA4             #x8046)
   (define GL_LUMINANCE12_ALPHA12            #x8047)
   (define GL_LUMINANCE16_ALPHA16            #x8048)
   (define GL_INTENSITY                      #x8049)
   (define GL_INTENSITY4                     #x804A)
   (define GL_INTENSITY8                     #x804B)
   (define GL_INTENSITY12                    #x804C)
   (define GL_INTENSITY16                    #x804D)
   (define GL_R3_G3_B2                       #x2A10)
   (define GL_RGB4                           #x804F)
   (define GL_RGB5                           #x8050)
   (define GL_RGB8                           #x8051)
   (define GL_RGB10                          #x8052)
   (define GL_RGB12                          #x8053)
   (define GL_RGB16                          #x8054)
   (define GL_RGBA2                          #x8055)
   (define GL_RGBA4                          #x8056)
   (define GL_RGB5_A1                        #x8057)
   (define GL_RGBA8                          #x8058)
   (define GL_RGB10_A2                       #x8059)
   (define GL_RGBA12                         #x805A)
   (define GL_RGBA16                         #x805B)

   (define GL_TEXTURE_RED_SIZE               #x805C)
   (define GL_TEXTURE_GREEN_SIZE             #x805D)
   (define GL_TEXTURE_BLUE_SIZE              #x805E)
   (define GL_TEXTURE_ALPHA_SIZE             #x805F)
   (define GL_TEXTURE_LUMINANCE_SIZE         #x8060)
   (define GL_TEXTURE_INTENSITY_SIZE         #x8061)

   (define GL_PROXY_TEXTURE_1D               #x8063)
   (define GL_PROXY_TEXTURE_2D               #x8064)

 ; C.7 EXT_copy_texture
   (define glCopyTexImage1D (GL GLvoid "glCopyTexImage1D" GLenum GLint GLenum GLint GLint GLsizei GLint))
   (define glCopyTexImage2D (GL GLvoid "glCopyTexImage2D" GLenum GLint GLenum GLint GLint GLsizei GLsizei GLint))
   (define glCopyTexSubImage1D (GL GLvoid "glCopyTexSubImage1D" GLenum GLint GLint GLint GLint GLsizei))
   (define glCopyTexSubImage2D (GL GLvoid "glCopyTexSubImage2D" GLenum GLint GLint GLint GLint GLint GLsizei GLsizei))

 ; C.7 EXT_subtexture
   (define glTexSubImage1D (GL GLvoid "glTexSubImage1D" GLenum GLint GLint GLsizei GLenum GLenum fft-any))
   (define glTexSubImage2D (GL GLvoid "glTexSubImage2D" GLenum GLint GLint GLint GLsizei GLsizei GLenum GLenum fft-any))
   ;glTexSubImage3D ; removed by 1.1 (introduced back by 1.2)

 ; C.8 EXT_texture_object
   (define glGenTextures (GL GLvoid "glGenTextures" GLsizei GLuint&))
   (define glDeleteTextures (GL GLvoid "glDeleteTextures" GLsizei GLuint*))
   (define glBindTexture (GL GLvoid "glBindTexture" GLenum GLuint))
   (define glPrioritizeTextures (GL GLvoid "glPrioritizeTextures" GLsizei GLuint* (fft* GLclampf)))
   (define glAreTexturesResident (GL GLboolean "glAreTexturesResident" GLsizei GLuint* GLboolean*))
   (define glIsTexture (GL GLboolean "glIsTexture" GLuint))

   (define GL_TEXTURE_PRIORITY   #x8066)
   (define GL_TEXTURE_RESIDENT   #x8067)
   (define GL_TEXTURE_BINDING_1D #x8068)
   (define GL_TEXTURE_BINDING_2D #x8069)

; ==========================================================================
; GLU
   (define GLU_VERSION_1_2 1)

))