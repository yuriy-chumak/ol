(define-library (lib gl 3.1 core)
(import
   (scheme core)
   (lib gl 3 context core)
   (lib gl 3 context 3.1)

   (lib gl)
   (OpenGL 3.1))

(export
   (exports (lib gl))
   (exports (OpenGL 3.1)))

(begin ))
