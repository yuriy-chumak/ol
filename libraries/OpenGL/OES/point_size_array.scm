; ===========================================================================
; OES_point_size_array                            (included in OpenGL ES 1.1)
;
;  https://www.khronos.org/registry/OpenGL/extensions/OES/OES_point_size_array.txt
;
; Version
;  Last Modifed Date: 23 Dec 2008
;
; Overview
;
(define-library (OpenGL OES point_size_array)

; ---------------------------------------------------------------------------
; Dependencies
(import (scheme core) (OpenGL ES platform))

; ---------------------------------------------------------------------------
(export OES_point_size_array

; ---------------------------------------------------------------------------
; New Procedures and Functions
   PointSizePointerOES ; void (enum type, sizei stride, const void *ptr)

; ---------------------------------------------------------------------------
; New Tokens
   POINT_SIZE_ARRAY_OES
   POINT_SIZE_ARRAY_TYPE_OES
   POINT_SIZE_ARRAY_STRIDE_OES
   POINT_SIZE_ARRAY_BUFFER_BINDING_OES
   POINT_SIZE_ARRAY_POINTER_OES
)

; ---------------------------------------------------------------------------
(begin
   (define OES_point_size_array (gl:QueryExtension "GL_OES_point_size_array"))

   (define PointSizePointerOES (if OES_point_size_array
      (GL GLvoid "PointSizePointerOES" GLenum GLsizei GLvoid*))) ; fft-any?

   (define POINT_SIZE_ARRAY_OES #x8B9C)
   (define POINT_SIZE_ARRAY_TYPE_OES #x898A)
   (define POINT_SIZE_ARRAY_STRIDE_OES #x898B)
   (define POINT_SIZE_ARRAY_BUFFER_BINDING_OES #x8B9F)
   (define POINT_SIZE_ARRAY_POINTER_OES #x898C)
))
