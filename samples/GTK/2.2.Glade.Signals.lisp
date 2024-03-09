#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "2.0.Glade.glade"))

((builder 'add-callback-symbol) "button_clicked"
   (GTK_CALLBACK (ptr userdata)
      (GtkMessageDialog (gtk_widget_get_toplevel ptr) {
         'flags GTK_DIALOG_MODAL
         'type  GTK_MESSAGE_INFO
         'message "Hello World!"
      })
      TRUE))
((builder 'connect-signals))

;; change button text
(((GtkButton ((builder 'get-object) "button"))
   'set-text) "Click Me!")

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'title "Glade Signals Example"
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