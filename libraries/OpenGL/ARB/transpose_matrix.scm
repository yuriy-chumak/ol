; ===========================================================================
; ARB_transpose_matrix                               (included in OpenGL 1.3)
;
;	https://www.khronos.org/registry/OpenGL/extensions/ARB/ARB_transpose_matrix.txt
;
; Version
;
; Overview
;
(define-library (OpenGL ARB transpose_matrix)

(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
; Dependencies


; ---------------------------------------------------------------------------
(export ARB_transpose_matrix

; ---------------------------------------------------------------------------
; New Procedures and Functions

   glLoadTransposeMatrixfARB
   glLoadTransposeMatrixdARB
   glMultTransposeMatrixfARB
   glMultTransposeMatrixdARB

; ---------------------------------------------------------------------------
; New Tokens

   GL_TRANSPOSE_MODELVIEW_MATRIX_ARB
   GL_TRANSPOSE_PROJECTION_MATRIX_ARB
   GL_TRANSPOSE_TEXTURE_MATRIX_ARB
   GL_TRANSPOSE_COLOR_MATRIX_ARB

)

; ---------------------------------------------------------------------------
(begin
   (define ARB_transpose_matrix (gl:QueryExtension "GL_ARB_transpose_matrix"))

   (setq GL gl:GetProcAddress)
   (define glLoadTransposeMatrixfARB (GL GLvoid "glLoadTransposeMatrixfARB" (fft* fft-float)))
   (define glLoadTransposeMatrixdARB (GL GLvoid "glLoadTransposeMatrixdARB" (fft* fft-double)))
   (define glMultTransposeMatrixfARB (GL GLvoid "glMultTransposeMatrixfARB" (fft* fft-float)))
   (define glMultTransposeMatrixdARB (GL GLvoid "glMultTransposeMatrixdARB" (fft* fft-double)))

   (define GL_TRANSPOSE_MODELVIEW_MATRIX_ARB  #x84E3)
   (define GL_TRANSPOSE_PROJECTION_MATRIX_ARB #x84E4)
   (define GL_TRANSPOSE_TEXTURE_MATRIX_ARB    #x84E5)
   (define GL_TRANSPOSE_COLOR_MATRIX_ARB      #x84E6)

))
