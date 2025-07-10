(define-library (lib gl 3.1)
(import
   (scheme core)
   (lib gl)
   (lib gl 3 context)
   (OpenGL 3.1))

(export
   (exports (lib gl))
   (exports (OpenGL 3.1)))
(begin
   (gl:set-context-version 3 1)
))
