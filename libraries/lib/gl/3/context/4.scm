(define-library (lib gl 3 context 4.0)
(import
   (scheme core)
   (lib gl)
   (lib gl 3 context))

(export )

(begin
   (gl:set-context-version 4 0)
))
