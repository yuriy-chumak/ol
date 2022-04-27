#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3))
(import (only (otus syscall) strftime))

; button handler, set label text to current date and time
(define print_datetime
   (GTK_CALLBACK (widget userdata)
      (print "Current label text: " (gtk_label_get_text userdata))
      (gtk_label_set_text userdata (string-append
         "I'm another UNICODE text from directly from a Lisp:"
         "\n"
         "別のUNICODEテキスト。"))
      TRUE))

(gtk_init '(0) #f)

(define builder (gtk_builder_new_from_file "templates/4. How to Deal With Strings.glade"))
(gtk_builder_connect_signals builder #f)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

(define label (gtk_builder_get_object builder "label1"))
(define button (gtk_builder_get_object builder "a_button"))
; set button press handler
(g_signal_connect button "clicked" (G_CALLBACK print_datetime) label)

(g_object_unref builder)

(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)
(gtk_main)
