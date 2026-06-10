(define-library (lib gl 4.1 compat)
(import
   (scheme core)
   (lib gl 3 context compat)
   (lib gl 3 context 4.1)

   (lib gl)
   (OpenGL 3.0)
   (OpenGL 4.1))

(export
   (exports (lib gl))
   (exports (OpenGL 3.0))
   (exports (OpenGL 4.1)))

(begin ))
