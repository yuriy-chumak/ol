#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3))
(import (only (otus syscall) strftime))

(define print_datetime (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (gtk_label_set_text userdata (strftime "%F %H:%M:%S\0"))
      TRUE
))))

; main:
(gtk_init '(0) #f)

; load and decode a file
(define builder (gtk_builder_new_from_file "3.1. Glade.glade"))
(gtk_builder_connect_signals builder #f)

; get window from template
(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

; get a button from template
(define button (gtk_builder_get_object builder "a_button"))

; get a label from template
(define label (gtk_builder_get_object builder "label1"))
(g_signal_connect button "clicked" (G_CALLBACK print_datetime) label)

; builder is no more required, let's free a system resource
(g_object_unref builder)

; close button processor
(define quit (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (print "Close pressed. Bye-bye.")
      ; when we do a gtk_main we should call a (gtk_main_quit)
      ; not a (g_application_quit)
      (gtk_main_quit))
)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; show window and run
(gtk_main)
