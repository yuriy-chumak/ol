; ==========================================================================
; GLX_ARB_create_context
;
;     https://www.khronos.org/registry/OpenGL/extensions/ARB/GLX_ARB_create_context.txt
;
; Version
;     Version 10, 2012/03/28
;
; Overview
;     ...
(define-library (OpenGL GLX ARB create_context)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import (OpenGL)
   (OpenGL GLX ARB get_proc_address))

; --------------------------------------------------------------------------
(export GLX_ARB_create_context

; --------------------------------------------------------------------------
; New Procedures and Functions

   glXCreateContextAttribsARB
   ;; GLXContext glXCreateContextAttribsARB(
   ;;                  Display *dpy, GLXFBConfig config,
   ;;                  GLXContext share_context, Bool direct,
   ;;                  const int *attrib_list);

; --------------------------------------------------------------------------
; New Tokens
;
   ;;  Accepted as an attribute name in <*attrib_list>:

   GLX_CONTEXT_MAJOR_VERSION_ARB
   GLX_CONTEXT_MINOR_VERSION_ARB
   GLX_CONTEXT_FLAGS_ARB
   GLX_CONTEXT_PROFILE_MASK_ARB

   ;;  Accepted as bits in the attribute value for GLX_CONTEXT_FLAGS_ARB in
   ;;  <*attrib_list>:

   GLX_CONTEXT_DEBUG_BIT_ARB
   GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB

   ;;  Accepted as bits in the attribute value for
   ;;  GLX_CONTEXT_PROFILE_MASK_ARB in <*attrib_list>:

   GLX_CONTEXT_CORE_PROFILE_BIT_ARB
   GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB
)

; --------------------------------------------------------------------------
(begin
   (define GLX_ARB_create_context (gl:QueryExtension "GLX_ARB_create_context"))

   (setq Display* fft-void*) ; X11
   (setq GLXContext fft-void*)
   (setq GLXFBConfig fft-void*)

   (define glXCreateContextAttribsARB (if GLX_ARB_create_context
      (glXGetProcAddressARB GLXContext "glXCreateContextAttribsARB"
               Display* GLXFBConfig GLXContext fft-int fft-int*)))

   (define GLX_CONTEXT_MAJOR_VERSION_ARB           #x2091)
   (define GLX_CONTEXT_MINOR_VERSION_ARB           #x2092)
   (define GLX_CONTEXT_FLAGS_ARB                   #x2094)
   (define GLX_CONTEXT_PROFILE_MASK_ARB            #x9126)

   (define GLX_CONTEXT_DEBUG_BIT_ARB               #x0001)
   (define GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB  #x0002)

   (define GLX_CONTEXT_CORE_PROFILE_BIT_ARB           #x00000001)
   (define GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB  #x00000002)

))
