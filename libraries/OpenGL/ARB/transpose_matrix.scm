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

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core)
   (OpenGL platform))

; ---------------------------------------------------------------------------
(export ARB_transpose_matrix

; ---------------------------------------------------------------------------
; New Procedures and Functions
   glLoadTransposeMatrixf
   glLoadTransposeMatrixd
   glMultTransposeMatrixf
   glMultTransposeMatrixd
; ---------------------------------------------------------------------------
; New Tokens
   GL_TRANSPOSE_MODELVIEW_MATRIX
   GL_TRANSPOSE_PROJECTION_MATRIX
   GL_TRANSPOSE_TEXTURE_MATRIX
   GL_TRANSPOSE_COLOR_MATRIX
)

; ---------------------------------------------------------------------------
(begin
   (define ARB_transpose_matrix (gl:QueryExtension "GL_ARB_transpose_matrix"))

   (define glLoadTransposeMatrixf (and ARB_transpose_matrix
      (gl:GetProcAddress GLvoid "glLoadTransposeMatrixf" (fft* fft-float))))
   (define glLoadTransposeMatrixd (and ARB_transpose_matrix
      (gl:GetProcAddress GLvoid "glLoadTransposeMatrixd" (fft* fft-double))))
   (define glMultTransposeMatrixf (and ARB_transpose_matrix
      (gl:GetProcAddress GLvoid "glMultTransposeMatrixf" (fft* fft-float))))
   (define glMultTransposeMatrixd (and ARB_transpose_matrix
      (gl:GetProcAddress GLvoid "glMultTransposeMatrixd" (fft* fft-double))))

   (define GL_TRANSPOSE_MODELVIEW_MATRIX  #x84E3)
   (define GL_TRANSPOSE_PROJECTION_MATRIX #x84E4)
   (define GL_TRANSPOSE_TEXTURE_MATRIX    #x84E5)
   (define GL_TRANSPOSE_COLOR_MATRIX      #x84E6)

))
