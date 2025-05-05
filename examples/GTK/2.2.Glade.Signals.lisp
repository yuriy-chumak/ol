#!/usr/bin/env ol
(import (gtk-3)
   (lib glib-2)
   (lib gtk-3 message-dialog))

;; explicitly init
(Gtk:init)

;; load ui from the file
(define builder
   (GtkBuilder "2.0.Glade.glade"))

((builder 'add-callback-symbol) "button_clicked"
   (GTK_CALLBACK (ptr userdata)
      (define widget (GtkWidget ptr))
      (define topptr ((widget 'get-toplevel)))
      (GtkMessageDialog topptr {
         'flags GTK_DIALOG_MODAL
         'type  GTK_MESSAGE_INFO
         'message "Hello World!"
      })
      TRUE))
((builder 'connect-signals))

;; change button text
(define button (GtkButton ((builder 'get-object) "button")))
((button 'set-text) "Click Me!")

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'title "Glade Signals Example"
      'on-destroy (lambda (this)
         (print "Close pressed. Bye-bye.")
         ; when we do a (gtk_main) we should use (gtk_main_quit)
         ;   instead of (g_application_quit)
         (Gtk:quit))
   }))

;; show it
((window 'show-all))

;; run
(Gtk:main)