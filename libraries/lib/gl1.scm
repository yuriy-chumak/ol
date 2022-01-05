(define-library (lib gl1)
(import
   (scheme core)
   (lib gl) (OpenGL version-1-4))
(export
   (exports (lib gl))
   (exports (OpenGL version-1-4)))
   
(begin
   (import (owl io))
   (print-to stderr "(lib gl1) is deprecated. Use (lib gl-1) instead!")
))
