; ===========================================================================
; ARB_point_parameters                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_point_parameters.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB point_parameters)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
(import
   (OpenGL 1.0))

; ---------------------------------------------------------------------------
(export ARB_point_parameters

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glPointParameterfARB
   glPointParameterfvARB

; ---------------------------------------------------------------------------
; New Tokens

   GL_POINT_SIZE_MIN_ARB
   GL_POINT_SIZE_MAX_ARB
   GL_POINT_FADE_THRESHOLD_SIZE_ARB
   GL_POINT_DISTANCE_ATTENUATION_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_point_parameters (gl:QueryExtension "GL_ARB_point_parameters"))

   (define GL gl:GetProcAddress)
   (define glPointParameterfARB (GL GLvoid "glPointParameterfARB" GLenum GLfloat))
   (define glPointParameterfvARB (GL GLvoid "glPointParameterfvARB" GLenum GLfloat*))

   (define GL_POINT_SIZE_MIN_ARB              #x8126)
   (define GL_POINT_SIZE_MAX_ARB              #x8127)
   (define GL_POINT_FADE_THRESHOLD_SIZE_ARB   #x8128)
   (define GL_POINT_DISTANCE_ATTENUATION_ARB  #x8129)

))
