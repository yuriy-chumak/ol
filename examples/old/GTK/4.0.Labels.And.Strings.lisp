#!/usr/bin/env ol
(import (lib glib-2)
   (lib gdk-3)
   (lib gtk-3)
   (lib gtk-3 message-dialog))

;; explicitly init
(gtk_init)

;; load ui from the file
(define builder
   (GtkBuilder "4.0.Labels.And.Strings.glade"))

;; name output label
(define output-label (GtkLabel
   ((builder 'get-object) "output-label")))

; mouse click handler
((builder 'add-callback-symbol) "clicked" (GTK_CALLBACK (widget event userdata)
   (define label (GtkLabel userdata))
   ((output-label 'set-text) (string-append
      "Reversed string:\n\t"
      (list->string (reverse (string->list ((label 'get-text)))))))
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
;; show it
((window 'show-all))

;; run
(gtk_main)