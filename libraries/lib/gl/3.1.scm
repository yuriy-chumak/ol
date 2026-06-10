; core profile!
; does not contain opengl 1.x and opengl 2.x functions
(define-library (lib gl 3.1)
(import
   (scheme core)
   (lib gl 3.1 core))

(export
   (exports (lib gl 3.1 core)))

(begin ))
