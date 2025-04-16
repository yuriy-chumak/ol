#!/usr/bin/env ol
(import (gtk-3))

;; application setup
(define (activate appl)
   ; create default window
   (define window (GtkWindow appl))

   ; show it
   ((window 'show-all)))

;; create an application
(define application (GtkApplication {
   'on-activate activate
}))

;; run
((application 'run) (command-line))
