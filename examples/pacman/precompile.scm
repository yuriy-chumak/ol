#!/usr/bin/env ol

(import (main))
(import (otus fasl))
(import (lang embed))

(fasl-save (make-entry
   (lambda (args)
      (born-blinky)
      ; in simplest case we can just return a vector of pinned functions
      [  (vm:pin (lambda () points))
         (vm:pin get-blinky)
         (vm:pin (lambda () (ref (syscall 1117) 1)))
         (vm:pin (lambda () (ref (syscall 1117) 3)))
         (vm:pin eat-the-point)
         (vm:pin blinky-move)
         (vm:pin get-level)
      ]))
   "tmp.bin")
