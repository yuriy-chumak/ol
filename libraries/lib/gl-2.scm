(define-library (lib gl-2)
(import
   (scheme base)
   (lib gl config)
   (lib gl)
   (OpenGL 2.1))
(export
   gl:create-program

   (exports (lib gl))
   (exports (OpenGL 2.1)))

(begin ))
