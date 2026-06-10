; just a regular opengl version
(define-library (lib gl 3.0)
(import
   (scheme core)
   (lib gl)
   (OpenGL 3.0))

(export
   (exports (lib gl))
   (exports (OpenGL 3.0)))

(begin ))
