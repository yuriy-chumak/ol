; ==========================================================================
; WGL_ARB_create_context
;
;     https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_create_context.txt
;
; Version
;     Version 20, 2009/07/28
;
; Overview
;     ...
(define-library (OpenGL WGL ARB create_context)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import
   (scheme core)
;  (OpenGL WGL ...)
   (OpenGL version-1-0))

; --------------------------------------------------------------------------
(export  WGL_ARB_create_context

; --------------------------------------------------------------------------
; New Procedures and Functions
   wglCreateContextAttribsARB
   ;; HGLRC wglCreateContextAttribsARB(HDC hDC, HGLRC hShareContext,
   ;;                                   const int *attribList)
   
; --------------------------------------------------------------------------
; New Tokens
;
   ;;  Accepted as an attribute name in <*attribList>:

   ;;      WGL_CONTEXT_MAJOR_VERSION_ARB           0x2091
   ;;      WGL_CONTEXT_MINOR_VERSION_ARB           0x2092
   ;;      WGL_CONTEXT_LAYER_PLANE_ARB             0x2093
   ;;      WGL_CONTEXT_FLAGS_ARB                   0x2094
   ;;      WGL_CONTEXT_PROFILE_MASK_ARB            0x9126

   ;;  Accepted as bits in the attribute value for WGL_CONTEXT_FLAGS in
   ;;  <*attribList>:

   ;;      WGL_CONTEXT_DEBUG_BIT_ARB               0x0001
   ;;      WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB  0x0002

   ;;  Accepted as bits in the attribute value for
   ;;  WGL_CONTEXT_PROFILE_MASK_ARB in <*attribList>:

   ;;      WGL_CONTEXT_CORE_PROFILE_BIT_ARB        0x00000001
   ;;      WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB 0x00000002

   ;;  New errors returned by GetLastError:

   ;;      ERROR_INVALID_VERSION_ARB               0x2095
   ;;      ERROR_INVALID_PROFILE_ARB               0x2096

)

; --------------------------------------------------------------------------
(begin
   (define WGL_ARB_create_context (gl:QueryExtension "WGL_ARB_create_context"))

   (setq HGLRC fft-void*)
   (setq HDC fft-void*)
   (setq HGLRC fft-void*)

   (define wglCreateContextAttribsARB (if WGL_ARB_create_context
      (gl:GetProcAddress HGLRC "wglCreateContextAttribsARB"
               HDC HGLRC fft-int*)))

   (define WGL_CONTEXT_MAJOR_VERSION_ARB           #x2091)
   (define WGL_CONTEXT_MINOR_VERSION_ARB           #x2092)
   (define WGL_CONTEXT_LAYER_PLANE_ARB             #x2093)
   (define WGL_CONTEXT_FLAGS_ARB                   #x2094)
   (define WGL_CONTEXT_PROFILE_MASK_ARB            #x9126)

   (define WGL_CONTEXT_DEBUG_BIT_ARB               #x0001)
   (define WGL_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB  #x0002)

   (define WGL_CONTEXT_CORE_PROFILE_BIT_ARB          #x00000001)
   (define WGL_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB #x00000002)

   (define ERROR_INVALID_VERSION_ARB               #x2095)
   (define ERROR_INVALID_PROFILE_ARB               #x2096)

))
