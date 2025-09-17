#!/usr/bin/env ol
(import (Gtk 3.0)
   (lib glib-2)
   (lib gtk-3 message-dialog))

;; explicitly init
(Gtk:init)

;; load ui from the file
(define builder (GtkBuilder "2.0.Glade.glade"))

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
((button 'set-text) "CLICK ME!")

;; setup main window
(define window ((builder 'get-Window) "window" {
   'title "Glade Signals Example"
   'on-destroy (lambda (this)
      (print "Close pressed. Bye-bye.")
      (Gtk:quit))
}))

;; show it
((window 'show-all))

;; run
(Gtk:main)
