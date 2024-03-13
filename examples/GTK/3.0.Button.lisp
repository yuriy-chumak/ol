#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "3.0.Button.glade"))

; button click handler
(define (clicked this)
   (print "button clicked")
   (GtkMessageDialog (gtk_widget_get_toplevel (this 'widget)) {
      'flags GTK_DIALOG_MODAL
      'type  GTK_MESSAGE_INFO
      'message "Hello World!"
   }))

; the button to be clicked
(define button (GtkButton
   ((builder 'get-object) "button") {
      'on-click clicked
}))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'width 320 'height 180
      'on-destroy (lambda (this)
         (gtk_main_quit))
   }))

;; show it
((window 'show-all))

;; run
(gtk_main)