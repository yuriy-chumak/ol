; ==========================================================================
; ARB_depth_buffer_float
;
; https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_depth_buffer_float.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB depth_buffer_float)

; --------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL))

; --------------------------------------------------------------------------
(export ARB_depth_buffer_float

; --------------------------------------------------------------------------
; New Procedures and Functions

; --------------------------------------------------------------------------
; New Tokens

;    Accepted by the <internalformat> parameter of TexImage1D, TexImage2D,
;    TexImage3D, CopyTexImage1D, CopyTexImage2D, and RenderbufferStorageEXT,
;    and returned in the <data> parameter of GetTexLevelParameter and
;    GetRenderbufferParameterivEXT:
   DEPTH_COMPONENT32F
   DEPTH32F_STENCIL8

;    Accepted by the <type> parameter of DrawPixels, ReadPixels, TexImage1D,
;    TexImage2D, TexImage3D, TexSubImage1D, TexSubImage2D, TexSubImage3D, and
;    GetTexImage:

   FLOAT_32_UNSIGNED_INT_24_8_REV

)

; --------------------------------------------------------------------------
(begin
   (define ARB_depth_buffer_float (gl:QueryExtension "GL_ARB_depth_buffer_float"))

   (define DEPTH_COMPONENT32F              #x8CAC)
   (define DEPTH32F_STENCIL8               #x8CAD)
   (define FLOAT_32_UNSIGNED_INT_24_8_REV  #x8DAD)

))
