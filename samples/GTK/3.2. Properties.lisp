#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3))
(import (only (otus syscall) strftime))

; button handler, set label text to current date and time
(define print_datetime (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (gtk_label_set_text userdata (strftime "%F %H:%M:%S\0"))
      TRUE)
)))

(gtk_init '(0) #f)

(define builder (gtk_builder_new_from_file "templates/3.2. Properties.glade"))
(gtk_builder_connect_signals builder #f)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

(define label (gtk_builder_get_object builder "label1"))
(define button (gtk_builder_get_object builder "a_button"))
; set button press handler
(g_signal_connect button "clicked" (G_CALLBACK print_datetime) label)

(g_object_unref builder)

(define quit (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit))
)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)
(gtk_main)
