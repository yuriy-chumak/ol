; ===========================================================================
; ARB_occlusion_query                                (included in OpenGL 1.5)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_occlusion_query.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB occlusion_query)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies


; ---------------------------------------------------------------------------
(export ARB_occlusion_query

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glGenQueriesARB
   glDeleteQueriesARB
   glIsQueryARB
   glBeginQueryARB
   glEndQueryARB
   glGetQueryivARB
   glGetQueryObjectivARB
   glGetQueryObjectuivARB

; ---------------------------------------------------------------------------
; New Tokens

   GL_SAMPLES_PASSED_ARB

   GL_QUERY_COUNTER_BITS_ARB
   GL_CURRENT_QUERY_ARB

   GL_QUERY_RESULT_ARB
   GL_QUERY_RESULT_AVAILABLE_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_occlusion_query (gl:QueryExtension "GL_ARB_occlusion_query"))

   (setq GL gl:GetProcAddress)
   (define glGenQueriesARB (GL GLvoid "GenQueriesARB" GLsizei GLuint*))
   (define glDeleteQueriesARB (GL GLvoid "DeleteQueriesARB" GLsizei GLuint*))
   (define glIsQueryARB (GL GLboolean "IsQueryARB" GLuint))
   (define glBeginQueryARB (GL GLvoid "BeginQueryARB" GLenum GLuint))
   (define glEndQueryARB (GL GLvoid "EndQueryARB" GLenum))
   (define glGetQueryivARB (GL GLvoid "GetQueryivARB" GLenum GLenum GLint*))
   (define glGetQueryObjectivARB (GL GLvoid "GetQueryObjectivARB" GLuint GLenum GLint*))
   (define glGetQueryObjectuivARB (GL GLvoid "GetQueryObjectuivARB" GLuint GLenum GLuint*))

   (define GL_SAMPLES_PASSED_ARB                             #x8914)
   (define GL_QUERY_COUNTER_BITS_ARB                         #x8864)
   (define GL_CURRENT_QUERY_ARB                              #x8865)
   (define GL_QUERY_RESULT_ARB                               #x8866)
   (define GL_QUERY_RESULT_AVAILABLE_ARB                     #x8867)
))
