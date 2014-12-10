; OpenGL 1.1 (1997)

(import         (OpenGL version-1-0))
(define-library (OpenGL version-1-1)
  (export
    GL_VERSION_1_1
    

  )
  
  (import
    (owl defmac) (owl io)
    (owl pinvoke)
    (OpenGL version-1-0))
  (begin
;  (import (OpenGL version-1-0))

(define    GL_VERSION_1_1    1)
(define % (dlopen GL_LIBRARY 0))

; 1.1
;(define GLbyte  ?) ; typedef signed char GLbyte
;(define GLshort ?) ; typedef short GLshort
;(define GLushort ?); typedef unsigned short GLushort
;typedef float GLclampf
;typedef double GLclampd

))
