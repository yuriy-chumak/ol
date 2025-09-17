#!/usr/bin/env ol
(import (Gtk 3.0))

;; explicit init
(Gtk:init)

;; load ui from the file
(define builder (GtkBuilder "2.0.Glade.glade"))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'on-destroy (lambda (this)
      (print "Close pressed. Bye-bye.")
      ; when we do a (Gtk:main) we should use (Gtk:quit)
      ;   instead of (GtkApplication 'quit)
      (Gtk:quit))
}))

;; show it
((window 'show-all))

;; run
(Gtk:main)
