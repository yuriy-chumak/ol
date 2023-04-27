; ===========================================================================
; EXT_fog_coord                                      (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_fog_coord.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT fog_coord)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.1))

; ---------------------------------------------------------------------------
(export EXT_fog_coord

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glFogCoordfEXT
   glFogCoorddEXT
   glFogCoordfvEXT
   glFogCoorddvEXT
   glFogCoordPointerEXT

; ---------------------------------------------------------------------------
; New Tokens

   GL_FOG_COORDINATE_SOURCE_EXT
   GL_FOG_COORDINATE_EXT
   GL_FRAGMENT_DEPTH_EXT
   GL_CURRENT_FOG_COORDINATE_EXT
   GL_FOG_COORDINATE_ARRAY_TYPE_EXT
   GL_FOG_COORDINATE_ARRAY_STRIDE_EXT
   GL_FOG_COORDINATE_ARRAY_POINTER_EXT
   GL_FOG_COORDINATE_ARRAY_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_fog_coord (gl:QueryExtension "GL_EXT_fog_coord"))

   (define GL gl:GetProcAddress)
   (define glFogCoordfEXT (GL GLvoid "glFogCoordfEXT" GLfloat))
   (define glFogCoorddEXT (GL GLvoid "glFogCoorddEXT" GLdouble))
   (define glFogCoordfvEXT (GL GLvoid "glFogCoordfvEXT" GLfloat*))
   (define glFogCoorddvEXT (GL GLvoid "glFogCoorddvEXT" GLdouble*))
   (define glFogCoordPointerEXT (GL GLvoid "glFogCoordPointerEXT" GLenum GLsizei fft-any))

   (define GL_FOG_COORDINATE_SOURCE_EXT           #x8450)
   (define GL_FOG_COORDINATE_EXT                  #x8451)
   (define GL_FRAGMENT_DEPTH_EXT                  #x8452)
   (define GL_CURRENT_FOG_COORDINATE_EXT          #x8453)
   (define GL_FOG_COORDINATE_ARRAY_TYPE_EXT       #x8454)
   (define GL_FOG_COORDINATE_ARRAY_STRIDE_EXT     #x8455)
   (define GL_FOG_COORDINATE_ARRAY_POINTER_EXT    #x8456)
   (define GL_FOG_COORDINATE_ARRAY_EXT            #x8457)

))
