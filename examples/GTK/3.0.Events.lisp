#!/usr/bin/env ol
(import (lib glib-2)
   (lib gdk-3)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "3.0.Events.glade"))

; register custom event (described in glade)
((builder 'add-callback-symbol) "mouse-clicked" (GTK_CALLBACK (widget event userdata)
   (define xy (((GdkEvent event) 'get-coords)))
   (when xy
      ; message dialog can have string format
      (GtkMessageDialog (gtk_widget_get_toplevel widget) {
            'type  GTK_MESSAGE_INFO
            'message "mouse clicked at:\n\nx = %f\ny = %f"
         }
         (car xy) (cdr xy) ))
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