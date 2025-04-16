#!/usr/bin/env ol
(import (gtk-3))

;; application setup
(define (activate appl)
   ; create customized window with title
   (define window (GtkWindow appl {
      'title "Gtk-3 Window"
      'width 640  'height 360
      'icon "dialog-information"
   }))

   ; show it
   ((window 'show-all)))

;; create an application
(define application (GtkApplication {
   'on-activate activate
}))

;; run
((application 'run) (command-line))
