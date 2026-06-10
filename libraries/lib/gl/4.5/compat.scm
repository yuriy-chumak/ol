(define-library (lib gl 4.5 compat)
(import
   (scheme core)
   (lib gl 3 context compat)
   (lib gl 3 context 4.5)

   (lib gl)
   (OpenGL 3.0)
   (OpenGL 4.5))

(export
   (exports (lib gl))
   (exports (OpenGL 3.0))
   (exports (OpenGL 4.5)))

(begin ))
