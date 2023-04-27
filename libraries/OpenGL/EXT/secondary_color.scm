; ===========================================================================
; EXT_secondary_color                                (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_secondary_color.txt
;
; Version
;
; Overview
;
(define-library (OpenGL EXT secondary_color)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL EXT separate_specular_color))

; ---------------------------------------------------------------------------
(export EXT_secondary_color

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glSecondaryColor3bEXT
   glSecondaryColor3bvEXT
   glSecondaryColor3dEXT
   glSecondaryColor3dvEXT
   glSecondaryColor3fEXT
   glSecondaryColor3fvEXT
   glSecondaryColor3iEXT
   glSecondaryColor3ivEXT
   glSecondaryColor3sEXT
   glSecondaryColor3svEXT
   glSecondaryColor3ubEXT
   glSecondaryColor3ubvEXT
   glSecondaryColor3uiEXT
   glSecondaryColor3uivEXT
   glSecondaryColor3usEXT
   glSecondaryColor3usvEXT
   glSecondaryColorPointerEXT

; ---------------------------------------------------------------------------
; New Tokens

   GL_COLOR_SUM_EXT
   GL_CURRENT_SECONDARY_COLOR_EXT
   GL_SECONDARY_COLOR_ARRAY_SIZE_EXT
   GL_SECONDARY_COLOR_ARRAY_TYPE_EXT
   GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT
   GL_SECONDARY_COLOR_ARRAY_POINTER_EXT
   GL_SECONDARY_COLOR_ARRAY_EXT

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_secondary_color (gl:QueryExtension "GL_EXT_secondary_color"))

   (define GL gl:GetProcAddress)
   (define glSecondaryColor3bEXT (GL GLvoid "glSecondaryColor3bEXT" GLbyte GLbyte GLbyte))
   (define glSecondaryColor3bvEXT (GL GLvoid "glSecondaryColor3bvEXT" GLbyte*))
   (define glSecondaryColor3dEXT (GL GLvoid "glSecondaryColor3dEXT" GLdouble GLdouble GLdouble))
   (define glSecondaryColor3dvEXT (GL GLvoid "glSecondaryColor3dvEXT" GLdouble*))
   (define glSecondaryColor3fEXT (GL GLvoid "glSecondaryColor3fEXT" GLfloat GLfloat GLfloat))
   (define glSecondaryColor3fvEXT (GL GLvoid "glSecondaryColor3fvEXT" GLfloat*))
   (define glSecondaryColor3iEXT (GL GLvoid "glSecondaryColor3iEXT" GLint GLint GLint))
   (define glSecondaryColor3ivEXT (GL GLvoid "glSecondaryColor3ivEXT" GLint*))
   (define glSecondaryColor3sEXT (GL GLvoid "glSecondaryColor3sEXT" GLshort GLshort GLshort))
   (define glSecondaryColor3svEXT (GL GLvoid "glSecondaryColor3svEXT" GLshort*))
   (define glSecondaryColor3ubEXT (GL GLvoid "glSecondaryColor3ubEXT" GLubyte GLubyte GLubyte))
   (define glSecondaryColor3ubvEXT (GL GLvoid "glSecondaryColor3ubvEXT" GLubyte*))
   (define glSecondaryColor3uiEXT (GL GLvoid "glSecondaryColor3uiEXT" GLuint GLuint GLuint))
   (define glSecondaryColor3uivEXT (GL GLvoid "glSecondaryColor3uivEXT" GLuint*))
   (define glSecondaryColor3usEXT (GL GLvoid "glSecondaryColor3usEXT" GLushort GLushort GLushort))
   (define glSecondaryColor3usvEXT (GL GLvoid "glSecondaryColor3usvEXT" GLushort*))
   (define glSecondaryColorPointerEXT (GL GLvoid "glSecondaryColorPointerEXT" GLint GLenum GLsizei fft-any))

   (define GL_COLOR_SUM_EXT                       #x8458)
   (define GL_CURRENT_SECONDARY_COLOR_EXT         #x8459)
   (define GL_SECONDARY_COLOR_ARRAY_SIZE_EXT      #x845A)
   (define GL_SECONDARY_COLOR_ARRAY_TYPE_EXT      #x845B)
   (define GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT    #x845C)
   (define GL_SECONDARY_COLOR_ARRAY_POINTER_EXT   #x845D)
   (define GL_SECONDARY_COLOR_ARRAY_EXT           #x845E)
))
