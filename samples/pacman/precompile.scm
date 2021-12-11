#!/usr/bin/env ol

(import (main))
(import (lang threading))
(import (otus fasl))

(fasl-save (vm:new 63 (lambda (args)
      ; our code use coroutines, so we should start a thread controller.
      (start-thread-controller
         (list ;1 thread
            ['just-a-name
               (lambda ()
                  (born-blinky)
                  ; in simplest case we can just return a vector of pinned functions
                  (halt [
                     (vm:pin (lambda () points))
                     (vm:pin get-blinky)
                     (vm:pin (lambda () (ref (syscall 1117) 1)))
                     (vm:pin (lambda () (ref (syscall 1117) 3)))
                     (vm:pin eat-the-point)
                     (vm:pin blinky-move)
                     (vm:pin get-level)
                  ])) ]))))
   "tmp.bin")
