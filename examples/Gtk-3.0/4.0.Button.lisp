#!/usr/bin/env ol
(import (Gtk 3.0))

;; explicit init
(Gtk:init)

;; load ui from the file
(define builder (GtkBuilder "4.0.Button.glade"))

;; button handler
(define button ((builder 'get-Button) "button" {
   'on-click (lambda (this)
      ((this 'set-markup)
         (string-append "<span fgcolor='green' weight='bold'>" ((this 'get-text)) "</span>")))
}))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'on-destroy (lambda (this)
      (print "Close pressed. Bye-bye.")
      ; when we do a (Gtk:main) we should use (Gtk:quit)
      ;   instead of (GtkApplication 'quit)
      (Gtk:quit))
}))

;; display the window
((window 'show-all))

;; run
(Gtk:main)
