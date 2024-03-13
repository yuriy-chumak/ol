#!/usr/bin/env ol
(import (lib glib-2)
   (only (lib gdk-3) gdk_event_get_coords)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "3.0.Events.glade"))

; register custom event (described in glade)
((builder 'add-callback-symbol) "mouse-clicked" (GTK_CALLBACK (widget event userdata)
   (define x '(#i0))
   (define y '(#i0))
   (if (gdk_event_get_coords event x y)
      (GtkMessageDialog (gtk_widget_get_toplevel widget) {
         'type  GTK_MESSAGE_INFO
         'message (string-append
               "mouse clicked at"  "\n"
               (number->string (car x)) ", " (number->string (car y)))
      }))
   TRUE))

;; setup main window
(define window (GtkWindow
   ((builder 'get-object) "window") {
      'width 320 'height 180
      'on-destroy (lambda (this)
         (gtk_main_quit))
   }))

;; connect events and callback-symbols
((builder 'connect-signals))
;; show main window
((window 'show-all))

;; run
(gtk_main)