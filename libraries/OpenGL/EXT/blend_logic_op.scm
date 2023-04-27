; ===========================================================================
; EXT_blend_logic_op                                 (included in OpenGL 1.1)
;
;    Fragment colors may be blended into the framebuffer using bitwise operations.
;
;    https://www.khronos.org/registry/OpenGL/extensions/EXT/EXT_blend_logic_op.txt
;
; Version
;    $Date: 1995/03/31 04:40:24 $ $Revision: 1.4 $
;
; Overview
;    A single additional blending equation is specified using the interface
;    defined by EXT_blend_minmax.  This equation is a simple logical
;    combination of the source and destination colors, where the specific
;    logical operation is as specified by LogicOp.  While only the XOR
;    operation may find wide application, the generality of full logical
;    operations is allowed.
(define-library (OpenGL EXT blend_logic_op)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies
;    EXT_blend_minmax affects the definition of this extension

; ---------------------------------------------------------------------------
(export EXT_blend_logic_op

; ---------------------------------------------------------------------------
; New Procedures and Functions
;    None

; ---------------------------------------------------------------------------
; New Tokens
;    None

)

; ---------------------------------------------------------------------------
(begin
   (define EXT_blend_logic_op (gl:QueryExtension "GL_EXT_blend_logic_op"))

))
