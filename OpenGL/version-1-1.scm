; OpenGL 1.1 (1997)

(import         (OpenGL version-1-0))
(define-library (OpenGL version-1-1)
  (export
    GL_VERSION_1_1
    

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke)
    (OpenGL version-1-0))
  (begin
;  (import (OpenGL version-1-0))

(define    GL_VERSION_1_1    1)
(define % (dlopen GL_LIBRARY 0))

; 1.1
;(define GLbyte  ?) ; typedef signed char GLbyte
;(define GLshort ?) ; typedef short GLshort
;(define GLushort ?); typedef unsigned short GLushort
;typedef float GLclampf
;typedef double GLclampd


		(define GL_DOUBLE #x140A)
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
;WINGDIAPI void APIENTRY glBindTexture (GLenum target, GLuint texture);
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

))
