; OpenGL 1.1 (1997)
; в комментриях указаны расширения, которые были интегрированы в этот выпуск
; ==========================================================================
(define-library (OpenGL version-1-1)
(export
       (exports (OpenGL version-1-0))
   GL_VERSION_1_1

;/* ClientArrayType */
;/*      GL_VERTEX_ARRAY */
;/*      GL_NORMAL_ARRAY */
;/*      GL_COLOR_ARRAY */
;/*      GL_INDEX_ARRAY */
;/*      GL_TEXTURE_COORD_ARRAY */
;/*      GL_EDGE_FLAG_ARRAY */

;/* ColorPointerType */
;/*      GL_BYTE */
;/*      GL_UNSIGNED_BYTE */
;/*      GL_SHORT */
;/*      GL_UNSIGNED_SHORT */
;/*      GL_INT */
;/*      GL_UNSIGNED_INT */
;/*      GL_FLOAT */
;/*      GL_DOUBLE */

;/* DataType */
;#define GL_BYTE                           0x1400
;#define GL_UNSIGNED_BYTE                  0x1401
;#define GL_SHORT                          0x1402
;#define GL_UNSIGNED_SHORT                 0x1403
;#define GL_INT                            0x1404
;#define GL_UNSIGNED_INT                   0x1405
;#define GL_FLOAT                          0x1406
;#define GL_2_BYTES                        0x1407
;#define GL_3_BYTES                        0x1408
;#define GL_4_BYTES                        0x1409
;#define GL_DOUBLE                         0x140A


;   ;glDisable
;   ; mode
;      GL_INDEX_LOGIC_OP
;      GL_COLOR_LOGIC_OP
;/*      GL_VERTEX_ARRAY */
;/*      GL_NORMAL_ARRAY */
;/*      GL_COLOR_ARRAY */
;/*      GL_INDEX_ARRAY */
;/*      GL_TEXTURE_COORD_ARRAY */
;/*      GL_EDGE_FLAG_ARRAY */
;/*      GL_POLYGON_OFFSET_POINT */
;/*      GL_POLYGON_OFFSET_LINE */
;/*      GL_POLYGON_OFFSET_FILL */

;/* GetPointerTarget */
;/*      GL_VERTEX_ARRAY_POINTER */
;/*      GL_NORMAL_ARRAY_POINTER */
;/*      GL_COLOR_ARRAY_POINTER */
;/*      GL_INDEX_ARRAY_POINTER */
;/*      GL_TEXTURE_COORD_ARRAY_POINTER */
;/*      GL_EDGE_FLAG_ARRAY_POINTER */



; -------------------


    ; todo: move this to the right place
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
      ;GL_RGB2
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

;WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
;WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
;WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
;WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
;WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
;WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
;WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
;WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
;WINGDIAPI void APIENTRY glArrayElement (GLint i);
;WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
;WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
;WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
;WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
;WINGDIAPI void APIENTRY glPopClientAttrib (void);
;WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);

)

; ---------------------------------------------------------------------------
   (import
      (r5rs base) (owl io)
      (OpenGL version-1-0))

; ---------------------------------------------------------------------------
(begin
   (define GL_VERSION_1_1 1)

#|
/* ClientArrayType */
/*      GL_VERTEX_ARRAY */
/*      GL_NORMAL_ARRAY */
/*      GL_COLOR_ARRAY */
/*      GL_INDEX_ARRAY */
/*      GL_TEXTURE_COORD_ARRAY */
/*      GL_EDGE_FLAG_ARRAY */

/* ColorPointerType */
/*      GL_BYTE */
/*      GL_UNSIGNED_BYTE */
/*      GL_SHORT */
/*      GL_UNSIGNED_SHORT */
/*      GL_INT */
/*      GL_UNSIGNED_INT */
/*      GL_FLOAT */
/*      GL_DOUBLE */

/* DataType */
#define GL_BYTE                           0x1400
#define GL_UNSIGNED_BYTE                  0x1401
#define GL_SHORT                          0x1402
#define GL_UNSIGNED_SHORT                 0x1403
#define GL_INT                            0x1404
#define GL_UNSIGNED_INT                   0x1405
#define GL_FLOAT                          0x1406
#define GL_2_BYTES                        0x1407
#define GL_3_BYTES                        0x1408
#define GL_4_BYTES                        0x1409
#define GL_DOUBLE                         0x140A

/*      GL_INDEX_LOGIC_OP */
/*      GL_COLOR_LOGIC_OP */

/*      GL_VERTEX_ARRAY */
/*      GL_NORMAL_ARRAY */
/*      GL_COLOR_ARRAY */
/*      GL_INDEX_ARRAY */
/*      GL_TEXTURE_COORD_ARRAY */
/*      GL_EDGE_FLAG_ARRAY */
/*      GL_POLYGON_OFFSET_POINT */
/*      GL_POLYGON_OFFSET_LINE */
/*      GL_POLYGON_OFFSET_FILL */

/* GetPointerTarget */
/*      GL_VERTEX_ARRAY_POINTER */
/*      GL_NORMAL_ARRAY_POINTER */
/*      GL_COLOR_ARRAY_POINTER */
/*      GL_INDEX_ARRAY_POINTER */
/*      GL_TEXTURE_COORD_ARRAY_POINTER */
/*      GL_EDGE_FLAG_ARRAY_POINTER */

#define GL_CLIENT_ATTRIB_STACK_DEPTH      0x0BB1
#define GL_INDEX_LOGIC_OP                 0x0BF1
#define GL_COLOR_LOGIC_OP                 0x0BF2
#define GL_MAX_CLIENT_ATTRIB_STACK_DEPTH  0x0D3B
#define GL_FEEDBACK_BUFFER_POINTER        0x0DF0
#define GL_FEEDBACK_BUFFER_SIZE           0x0DF1
#define GL_FEEDBACK_BUFFER_TYPE           0x0DF2
#define GL_SELECTION_BUFFER_POINTER       0x0DF3
#define GL_SELECTION_BUFFER_SIZE          0x0DF4
/*      GL_TEXTURE_BINDING_1D */
/*      GL_TEXTURE_BINDING_2D */
/*      GL_VERTEX_ARRAY */
/*      GL_NORMAL_ARRAY */
/*      GL_COLOR_ARRAY */
/*      GL_INDEX_ARRAY */
/*      GL_TEXTURE_COORD_ARRAY */
/*      GL_EDGE_FLAG_ARRAY */
/*      GL_VERTEX_ARRAY_SIZE */
/*      GL_VERTEX_ARRAY_TYPE */
/*      GL_VERTEX_ARRAY_STRIDE */
/*      GL_NORMAL_ARRAY_TYPE */
/*      GL_NORMAL_ARRAY_STRIDE */
/*      GL_COLOR_ARRAY_SIZE */
/*      GL_COLOR_ARRAY_TYPE */
/*      GL_COLOR_ARRAY_STRIDE */
/*      GL_INDEX_ARRAY_TYPE */
/*      GL_INDEX_ARRAY_STRIDE */
/*      GL_TEXTURE_COORD_ARRAY_SIZE */
/*      GL_TEXTURE_COORD_ARRAY_TYPE */
/*      GL_TEXTURE_COORD_ARRAY_STRIDE */
/*      GL_EDGE_FLAG_ARRAY_STRIDE */
/*      GL_POLYGON_OFFSET_FACTOR */
/*      GL_POLYGON_OFFSET_UNITS */

#define GL_TEXTURE_INTERNAL_FORMAT        0x1003
/*      GL_TEXTURE_RED_SIZE */
/*      GL_TEXTURE_GREEN_SIZE */
/*      GL_TEXTURE_BLUE_SIZE */
/*      GL_TEXTURE_ALPHA_SIZE */
/*      GL_TEXTURE_LUMINANCE_SIZE */
/*      GL_TEXTURE_INTENSITY_SIZE */
/*      GL_TEXTURE_PRIORITY */
/*      GL_TEXTURE_RESIDENT */

/*      GL_PHONG_HINT */

/* IndexPointerType */
/*      GL_SHORT */
/*      GL_INT */
/*      GL_FLOAT */
/*      GL_DOUBLE */

.......


|#
; 1.1
;(define GLbyte  ?) ; typedef signed char GLbyte
;(define GLshort ?) ; typedef short GLshort
;(define GLushort ?); typedef unsigned short GLushort
;typedef float GLclampf
;typedef double GLclampd

		(define GL_DOUBLE #x140A)
		
		(define GL_POLYGON_OFFSET #x8037)
		(define GL_POLYGON_OFFSET_FACTOR #x8038)
		(define GL_POLYGON_OFFSET_UNITS #x2A00)
		(define GL_POLYGON_OFFSET_POINT #x2A01)
		(define GL_POLYGON_OFFSET_LINE #x2A02)
		(define GL_POLYGON_OFFSET_FILL #x8037)
		(define GL_ALPHA4 #x803B)
		(define GL_ALPHA8 #x803C)
		(define GL_ALPHA12 #x803D)
		(define GL_ALPHA16 #x803E)
		(define GL_LUMINANCE4 #x803F)
		(define GL_LUMINANCE8 #x8040)
		(define GL_LUMINANCE12 #x8041)
		(define GL_LUMINANCE16 #x8042)
		(define GL_LUMINANCE4_ALPHA4 #x8043)
		(define GL_LUMINANCE6_ALPHA2 #x8044)
		(define GL_LUMINANCE8_ALPHA8 #x8045)
		(define GL_LUMINANCE12_ALPHA4 #x8046)
		(define GL_LUMINANCE12_ALPHA12 #x8047)
		(define GL_LUMINANCE16_ALPHA16 #x8048)
		(define GL_INTENSITY #x8049)
		(define GL_INTENSITY4 #x804A)
		(define GL_INTENSITY8 #x804B)
		(define GL_INTENSITY12 #x804C)
		(define GL_INTENSITY16 #x804D)
		(define GL_R3_G3_B2 #x2A10)
		(define GL_RGB4 #x804F)
		(define GL_RGB5 #x8050)
		(define GL_RGB8 #x8051)
		(define GL_RGB10 #x8052)
		(define GL_RGB12 #x8053)
		(define GL_RGB16 #x8054)
		(define GL_RGBA2 #x8055)
		(define GL_RGBA4 #x8056)
		(define GL_RGB5_A1 #x8057)
		(define GL_RGBA8 #x8058)
		(define GL_RGB10_A2 #x8059)
		(define GL_RGBA12 #x805A)
		(define GL_RGBA16 #x805B)
		(define GL_TEXTURE_RED_SIZE #x805C)
		(define GL_TEXTURE_GREEN_SIZE #x805D)
		(define GL_TEXTURE_BLUE_SIZE #x805E)
		(define GL_TEXTURE_ALPHA_SIZE #x805F)
		(define GL_TEXTURE_LUMINANCE_SIZE #x8060)
		(define GL_TEXTURE_INTENSITY_SIZE #x8061)
		(define GL_PROXY_TEXTURE_1D #x8063)
		(define GL_PROXY_TEXTURE_2D #x8064)
		(define GL_TEXTURE_TOO_LARGE #x8065)
		(define GL_TEXTURE_PRIORITY #x8066)
		(define GL_TEXTURE_RESIDENT #x8067)
		(define GL_TEXTURE_BINDING_1D #x8068)
		(define GL_TEXTURE_BINDING_2D #x8069)
		(define GL_CLIENT_PIXEL_STORE_BIT #x00000001)
		(define GL_CLIENT_VERTEX_ARRAY_BIT #x00000002)
		(define GL_CLIENT_ALL_ATTRIB_BITS #xFFFFFFFF)

;WINGDIAPI GLboolean APIENTRY glAreTexturesResident (GLsizei n, const GLuint *textures, GLboolean *residences);
;WINGDIAPI void APIENTRY glArrayElement (GLint i);
;WINGDIAPI void APIENTRY glColorPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glCopyTexImage1D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLint border);
;WINGDIAPI void APIENTRY glCopyTexImage2D (GLenum target, GLint level, GLenum internalFormat, GLint x, GLint y, GLsizei width, GLsizei height, GLint border);
;WINGDIAPI void APIENTRY glCopyTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLint x, GLint y, GLsizei width);
;WINGDIAPI void APIENTRY glCopyTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint x, GLint y, GLsizei width, GLsizei height);
;WINGDIAPI void APIENTRY glDeleteTextures (GLsizei n, const GLuint *textures);
;WINGDIAPI void APIENTRY glDisableClientState (GLenum array);
;WINGDIAPI void APIENTRY glDrawArrays (GLenum mode, GLint first, GLsizei count);
;WINGDIAPI void APIENTRY glDrawElements (GLenum mode, GLsizei count, GLenum type, const GLvoid *indices);
;WINGDIAPI void APIENTRY glEdgeFlagPointer (GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glEnableClientState (GLenum array);
;WINGDIAPI void APIENTRY glGenTextures (GLsizei n, GLuint *textures);
;WINGDIAPI void APIENTRY glGetPointerv (GLenum pname, GLvoid* *params);
;WINGDIAPI void APIENTRY glIndexPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glIndexub (GLubyte c);
;WINGDIAPI void APIENTRY glIndexubv (const GLubyte *c);
;WINGDIAPI void APIENTRY glInterleavedArrays (GLenum format, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI GLboolean APIENTRY glIsTexture (GLuint texture);
;WINGDIAPI void APIENTRY glNormalPointer (GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glPolygonOffset (GLfloat factor, GLfloat units);
;WINGDIAPI void APIENTRY glPopClientAttrib (void);
;WINGDIAPI void APIENTRY glPrioritizeTextures (GLsizei n, const GLuint *textures, const GLclampf *priorities);
;WINGDIAPI void APIENTRY glPushClientAttrib (GLbitfield mask);
;WINGDIAPI void APIENTRY glTexCoordPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);
;WINGDIAPI void APIENTRY glTexSubImage1D (GLenum target, GLint level, GLint xoffset, GLsizei width, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glTexSubImage2D (GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width, GLsizei height, GLenum format, GLenum type, const GLvoid *pixels);
;WINGDIAPI void APIENTRY glVertexPointer (GLint size, GLenum type, GLsizei stride, const GLvoid *pointer);

; integrate:
;   EXT_vertex_array
;   WIN_draw_range_elements
;   WIN_swap_hint
;   EXT_paletted_texture


(define GLU_VERSION_1_2                 1)
; add glu functions
))