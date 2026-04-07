#!/usr/bin/env ol
(import (Gtk 3.0))

;; application setup
(define (activate appl)
   ; create customized window with title
   (define window (GtkWindow appl {
      'title "Gtk-3 Window"
      'width 640  'height 360
      'icon "dialog-information"
   }))

   ; display the window
   ((window 'show-all)))

;; create the application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
