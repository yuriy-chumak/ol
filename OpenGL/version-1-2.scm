; OpenGL 1.2 (1998), OpenGL 1.2.1 (1998)
; в комментриях указаны расширения, которые были интегрированы в этот выпуск
; ==========================================================================
(define-library (OpenGL version-1-2)
(export
       (exports (OpenGL version-1-1))
   GL_VERSION_1_2


   )

   (import
      (r5rs base) (owl io)
      (OpenGL version-1-1))
      
   (import (OpenGL EXT bgra))
      
(begin
   (define GL_VERSION_1_2 1)
   (define $ (dlopen GL_LIBRARY))
   
   
;  ; opengl 1.2 https://www.opengl.org/registry/api/GL/glext.h

   (define GLU_VERSION_1_3 1)
))

; OpenGL 1.2.1 (1998)
(define-library (OpenGL version-1-2-1)
(export
       (exports (OpenGL version-1-2))
   GL_VERSION_1_2_1
   )
  
   (import
      (r5rs base) (owl io)
      (OpenGL version-1-2))

(begin
   (define GL_VERSION_1_2_1 1)
   (define $ (dlopen GL_LIBRARY))
))