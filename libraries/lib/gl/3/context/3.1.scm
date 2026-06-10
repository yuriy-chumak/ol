(define-library (lib gl 3 context 3.1)
(import
   (scheme core)
   (lib gl)
   (lib gl 3 context))

(export )

(begin
   (gl:set-context-version 3 1)
))
