#!/usr/bin/env ol
(import (Gtk 3.0))

;; application setup
(define (activate app)
   ; main application window
   (define window (GtkWindow app {
      'title "Gtk-3 Window With Content"
      'width 320  'height 180
      'icon "dialog-information"
   }))

   ; add a label to the window
   (define label (GtkLabel
      "Lorem ipsum dolor sit amet,\nconsectetur adipiscing elit."))
   ((window 'add) label)

   ; display the window (with content)
   ((window 'show-all)))

;; create the application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
