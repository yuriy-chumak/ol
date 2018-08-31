; ==========================================================================
; WGL_ARB_create_context
;
;     https://www.khronos.org/registry/OpenGL/extensions/ARB/WGL_ARB_create_context.txt
;
; Version
;     
;
; Overview
;     ...
(define-library (OpenGL WGL ARB create_context)

; --------------------------------------------------------------------------
; Dependencies
;     None
(import
   (scheme core)
   (OpenGL version-1-0))

; --------------------------------------------------------------------------
(export WGL_ARB_create_context

; --------------------------------------------------------------------------
; New Procedures and Functions

; --------------------------------------------------------------------------
; New Tokens
;
)

; --------------------------------------------------------------------------
(begin
   (define WGL_ARB_create_context (gl:QueryExtension "WGL_ARB_create_context"))

))
