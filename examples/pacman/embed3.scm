#!/usr/bin/env ol

(import (main)) ; game logic
(import (only (otus embed) return))

; last lambda is an entry point of compiled binary
(lambda args
   (born-blinky)

   ; just return a vector of functions
   ; without stopping internal coroutines
   (return [
      (lambda () points)
      get-blinky
      (lambda () (ref (syscall 1117) 1)) ; memory stats
      (lambda () (ref (syscall 1117) 3)) ; memory stats
      eat-the-point
      blinky-move
      get-level
   ]))
