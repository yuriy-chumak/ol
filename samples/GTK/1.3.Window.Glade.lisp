#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

;; explicitly init
(gtk_init '(0) #f)

;; load ui from the file
(define builder
   (GtkBuilder "templates/3.1. Glade.glade"))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window")
   { 'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)