#!/usr/bin/env ol
(import (Gtk 3.0))

;; application setup
(define (activate appl)
   ; create default window
   (define window (GtkWindow appl))

   ; display the window
   ((window 'show-all)))

;; create the application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
