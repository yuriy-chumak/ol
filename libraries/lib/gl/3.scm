(define-library (lib gl 3.0)
(import
   (scheme core)
   (lib gl)
   (lib gl 3 context)
   (OpenGL 3.0))

(export
   (exports (lib gl))
   (exports (OpenGL 3.0)))
(begin
   (gl:set-context-version 3 0)
))
