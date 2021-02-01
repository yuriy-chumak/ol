; ==========================================================================
; GLX_EXT_swap_control
;
;     https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_swap_control.txt
;
; Version
;     ...
;
; Overview
;     ...
(define-library (OpenGL GLX EXT swap_control)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import
   (scheme core)
   (OpenGL platform)
   (OpenGL GLX ARB get_proc_address))

; --------------------------------------------------------------------------
(export GLX_EXT_swap_control

; --------------------------------------------------------------------------
; New Procedures and Functions

   glXSwapIntervalEXT

; --------------------------------------------------------------------------
; New Tokens
;

   GLX_SWAP_INTERVAL_EXT
   GLX_MAX_SWAP_INTERVAL_EXT
)

; --------------------------------------------------------------------------
(begin
   (define GLX_EXT_swap_control (gl:QueryExtension "GLX_EXT_swap_control"))

   (setq Display* fft-void*)
   (setq GLXDrawable fft-void*)

   (define glXSwapIntervalEXT (if GLX_EXT_swap_control
      (glXGetProcAddressARB fft-void "glXSwapIntervalEXT"
               Display* GLXDrawable fft-int)))

   (define GLX_SWAP_INTERVAL_EXT               #x20F1)
   (define GLX_MAX_SWAP_INTERVAL_EXT           #x20F2)
))
