; ===========================================================================
; ARB_window_pos                                     (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_window_pos.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB window_pos)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.0))

; ---------------------------------------------------------------------------
(export ARB_window_pos

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glWindowPos2dARB
   glWindowPos2fARB
   glWindowPos2iARB
   glWindowPos2sARB
   glWindowPos2dvARB
   glWindowPos2fvARB
   glWindowPos2ivARB
   glWindowPos2svARB
   glWindowPos3dARB
   glWindowPos3fARB
   glWindowPos3iARB
   glWindowPos3sARB
   glWindowPos3dvARB
   glWindowPos3fvARB
   glWindowPos3ivARB
   glWindowPos3svARB

; ---------------------------------------------------------------------------
; New Tokens

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_window_pos (gl:QueryExtension "GL_ARB_window_pos"))

   (define GL gl:GetProcAddress)
   (define glWindowPos2dARB (GL GLvoid "glWindowPos2dARB" GLdouble GLdouble))
   (define glWindowPos2fARB (GL GLvoid "glWindowPos2fARB" GLfloat GLfloat))
   (define glWindowPos2iARB (GL GLvoid "glWindowPos2iARB" GLint GLint))
   (define glWindowPos2sARB (GL GLvoid "glWindowPos2sARB" GLshort GLshort))
   (define glWindowPos2dvARB (GL GLvoid "glWindowPos2dvARB" GLdouble*))
   (define glWindowPos2fvARB (GL GLvoid "glWindowPos2fvARB" GLfloat*))
   (define glWindowPos2ivARB (GL GLvoid "glWindowPos2ivARB" GLint*))
   (define glWindowPos2svARB (GL GLvoid "glWindowPos2svARB" GLshort*))
   (define glWindowPos3dARB (GL GLvoid "glWindowPos3dARB" GLdouble GLdouble GLdouble))
   (define glWindowPos3fARB (GL GLvoid "glWindowPos3fARB" GLfloat GLfloat GLfloat))
   (define glWindowPos3iARB (GL GLvoid "glWindowPos3iARB" GLint GLint GLint))
   (define glWindowPos3sARB (GL GLvoid "glWindowPos3sARB" GLshort GLshort GLshort))
   (define glWindowPos3dvARB (GL GLvoid "glWindowPos3dvARB" GLdouble*))
   (define glWindowPos3fvARB (GL GLvoid "glWindowPos3fvARB" GLfloat*))
   (define glWindowPos3ivARB (GL GLvoid "glWindowPos3ivARB" GLint*))
   (define glWindowPos3svARB (GL GLvoid "glWindowPos3svARB" GLshort*))

))
