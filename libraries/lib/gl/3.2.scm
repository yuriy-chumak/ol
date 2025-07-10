(define-library (lib gl 3.2)
(import
   (scheme core)
   (lib gl)
   (lib gl 3 context)
   (OpenGL 3.2))

(export
   (exports (lib gl))
   (exports (OpenGL 3.2)))
(begin
   (gl:set-context-version 3 2)
))
