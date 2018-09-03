; ==========================================================================
; GLX_ARB_get_proc_address
;
;     https://www.khronos.org/registry/OpenGL/extensions/ARB/GLX_ARB_get_proc_address.txt
;
; Version
;     Last Modified Date: January 3, 2000;
; Overview
;     ...
(define-library (OpenGL GLX ARB get_proc_address)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import (OpenGL))

; --------------------------------------------------------------------------
(export GLX_ARB_get_proc_address

; --------------------------------------------------------------------------
; New Procedures and Functions
   glXGetProcAddressARB ; void (*glXGetProcAddressARB(const GLubyte *procName))(...)
   
; --------------------------------------------------------------------------
; New Tokens
;
;  none

; --------------------------------------------------------------------------
; Issues
;
; * There's a recursion problem with this feature. The purpose of
;   GetProcAddressARB is to return pointers to extension functions and
;   GetProcAddressARB is itself such a function! This presents a
;   puzzle to the application developer.
;
;      Implementations must export the glXGetProcAddressARB entry point
;      statically.
)

; --------------------------------------------------------------------------
(begin
   (define GLX_ARB_get_proc_address (gl:QueryExtension "GLX_ARB_get_proc_address"))

(setq GLX (load-dynamic-library "libGL.so.1"))
(setq GetProcAddress (GLX type-vptr "glXGetProcAddressARB" type-string))

(define (glXGetProcAddressARB type name . prototype)
   (let ((rtty (cons type prototype))
         (function (GetProcAddress (c-string name))))
      (if function
      (lambda args
         (exec ffi function rtty args)))))
))
