(define-library (lib gl 3.2 core)
(import
   (scheme core)
   (lib gl 3 context core)
   (lib gl 3 context 3.2)

   (lib gl)
   (OpenGL 3.2))

(export
   (exports (lib gl))
   (exports (OpenGL 3.2)))

(begin ))
