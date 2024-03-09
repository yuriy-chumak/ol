#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "2.0.Glade.glade"))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         ; when we do a (gtk_main) we should use (gtk_main_quit)
         ;   instead of (g_application_quit)
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)