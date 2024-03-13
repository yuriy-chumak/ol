#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

;; application activate
(define (activate appl)
   ; create default window with a title
   (define window (GtkWindow appl "Window"))

   ; show it
   ((window 'show-all)))

;; create an application
(define app (GtkApplication {
   'on-activate activate
}))

;; run
((app 'run) (command-line))
