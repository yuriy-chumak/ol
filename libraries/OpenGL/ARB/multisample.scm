; ===========================================================================
; ARB_multisample                                    (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_multisample.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB multisample)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies

; ---------------------------------------------------------------------------
(export ARB_multisample

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glSampleCoverageARB

; ---------------------------------------------------------------------------
; New Tokens

   GLX_SAMPLE_BUFFERS_ARB
   GLX_SAMPLES_ARB

   WGL_SAMPLE_BUFFERS_ARB
   WGL_SAMPLES_ARB

   GL_MULTISAMPLE_ARB
   GL_SAMPLE_ALPHA_TO_COVERAGE_ARB
   GL_SAMPLE_ALPHA_TO_ONE_ARB
   GL_SAMPLE_COVERAGE_ARB

   GL_MULTISAMPLE_BIT_ARB

   GL_SAMPLE_BUFFERS_ARB
   GL_SAMPLES_ARB
   GL_SAMPLE_COVERAGE_VALUE_ARB
   GL_SAMPLE_COVERAGE_INVERT_ARB
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_multisample (gl:QueryExtension "GL_ARB_multisample"))

   (setq GL gl:GetProcAddress)
   (define glSampleCoverageARB (GL GLvoid "SampleCoverageARB" GLclampf GLboolean))

   (define GLX_SAMPLE_BUFFERS_ARB                  100000)
   (define GLX_SAMPLES_ARB                         100001)
   (define WGL_SAMPLE_BUFFERS_ARB                  #x2041)
   (define WGL_SAMPLES_ARB                         #x2042)

   (define GL_MULTISAMPLE_ARB                      #x809D)
   (define GL_SAMPLE_ALPHA_TO_COVERAGE_ARB         #x809E)
   (define GL_SAMPLE_ALPHA_TO_ONE_ARB              #x809F)
   (define GL_SAMPLE_COVERAGE_ARB                  #x80A0)
   (define GL_MULTISAMPLE_BIT_ARB                  #x20000000)
   (define GL_SAMPLE_BUFFERS_ARB                   #x80A8)
   (define GL_SAMPLES_ARB                          #x80A9)
   (define GL_SAMPLE_COVERAGE_VALUE_ARB            #x80AA)
   (define GL_SAMPLE_COVERAGE_INVERT_ARB           #x80AB)

))
