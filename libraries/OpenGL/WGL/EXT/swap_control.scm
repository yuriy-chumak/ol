; ==========================================================================
; WGL_EXT_swap_control
;
;     https://www.khronos.org/registry/OpenGL/extensions/EXT/WGL_EXT_swap_control.txt
;
; Version
;     Date: 9/23/1999   Revision: 1.5
;
; Overview
;     ...
(define-library (OpenGL WGL EXT swap_control)

; --------------------------------------------------------------------------
; Dependencies
;     WGL_EXT_extensions_string is required.
(import
   (scheme core)
   (OpenGL platform))

; --------------------------------------------------------------------------
(export  WGL_EXT_swap_control

; --------------------------------------------------------------------------
; New Procedures and Functions
   wglSwapIntervalEXT
   wglGetSwapIntervalEXT

; --------------------------------------------------------------------------
; New Tokens
;
;  None
)

; --------------------------------------------------------------------------
(begin
   (define WGL_EXT_swap_control (gl:QueryExtension "WGL_EXT_swap_control"))

   (setq BOOL fft-int)

   (define wglSwapIntervalEXT
      (gl:GetProcAddress BOOL "wglSwapIntervalEXT" fft-int))

   (define wglGetSwapIntervalEXT
      (gl:GetProcAddress fft-int "wglGetSwapIntervalEXT"))

))
