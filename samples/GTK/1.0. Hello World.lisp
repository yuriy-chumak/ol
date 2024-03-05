#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

; button click handler
(define (clicked this)
   (print "button clicked")
   (GtkMessageDialog (gtk_widget_get_toplevel (this 'widget)) {
      'flags GTK_DIALOG_MODAL
      'type  GTK_MESSAGE_INFO
      'message "Hello World!"
   }))

; application init
(define (activate app)

   ; main application window
   (define window (GtkWindow app {
      'title "1.0. Hello World"
      'width 320 'height 240
   }))

   ; the button to be clicked
   (define button (GtkButton {
      'text "I'm a BUTTON\n\nClick Me Now!"
      'on-click clicked
   }))
   (window 'add button)

   ; display the window and it's controls
   (window 'show-all))

;; create an application
(define app (GtkApplication {
   'id "org.gtk.example"
   'on-activate activate
}))

;; run
;  note: command line will not be changed by calling
(app 'run (command-line)) ; returns execution result
