; OpenGL 3.0 (2008)

(define-library (OpenGL version-3-0)
(export

   GL_VERSION_3_0

   GL_MAJOR_VERSION
   GL_MINOR_VERSION

   (exports (OpenGL version-1-0)))

(import (scheme core)
   (OpenGL version-1-0))

(begin
   (define GL_VERSION_3_0 1)

(define GL_MAJOR_VERSION                  #x821B)
(define GL_MINOR_VERSION                  #x821C)


; GLX context creation:

))