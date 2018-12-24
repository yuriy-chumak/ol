; ===========================================================================
; ARB_shadow                                         (included in OpenGL 1.4)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_shadow.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB shadow)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_shadow

; ---------------------------------------------------------------------------
; New Procedures and Functions

; ---------------------------------------------------------------------------
; New Tokens

;    Accepted by the <pname> parameter of TexParameterf, TexParameteri,
;    TexParameterfv, TexParameteriv, GetTexParameterfv, and GetTexParameteriv:

    GL_TEXTURE_COMPARE_MODE_ARB
    GL_TEXTURE_COMPARE_FUNC_ARB

;    Accepted by the <param> parameter of TexParameterf, TexParameteri,
;    TexParameterfv, and TexParameteriv when the <pname> parameter is
;    TEXTURE_COMPARE_MODE_ARB:

    GL_COMPARE_R_TO_TEXTURE_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_shadow (gl:QueryExtension "GL_ARB_shadow"))

   (define GL_TEXTURE_COMPARE_MODE_ARB    #x884C)
   (define GL_TEXTURE_COMPARE_FUNC_ARB    #x884D)
   (define GL_COMPARE_R_TO_TEXTURE_ARB    #x884E)

))
