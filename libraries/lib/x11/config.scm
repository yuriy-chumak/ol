(define-library (lib x11 config)
(export config)
(import (scheme core) (owl ff))
(begin
   (setq x86? (eq? (size (vm:cast 0 type-vptr)) 4))
   (define config {})
))
