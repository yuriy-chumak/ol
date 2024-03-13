#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

;; application activate
(define (activate appl)
   ; create customized window with title
   (define window (GtkWindow appl {
      'title "Customized Window"
      'width 640 'height 360
      'icon "dialog-information"
   }))

   ; show it
   ((window 'show-all)))

;; create an application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
