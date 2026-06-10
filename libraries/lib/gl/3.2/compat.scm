(define-library (lib gl 3.2 compat)
(import
   (scheme core)
   (lib gl 3 context compat)
   (lib gl 3 context 3.2)

   (lib gl)
   (OpenGL ARB compatibility)
   (OpenGL 3.2))

(export
   (exports (lib gl))
   (exports (OpenGL ARB compatibility))
   (exports (OpenGL 3.2)))

(begin ))
