#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gtk-3))
(import (only (otus syscall) strftime))

; main:
(gtk_init '(0) #f)

; load and decode a file
(define builder (gtk_builder_new_from_file "templates/5.1. AboutDialog.glade"))
(gtk_builder_connect_signals builder #f)

; get window from template
(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

; about dialog
(define about (gtk_builder_get_object builder "about"))

; get a button from template
(define button (gtk_builder_get_object builder "a_button"))
(define click (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (gtk_dialog_run about)
      TRUE)
)))
(g_signal_connect button "clicked" (G_CALLBACK click) NULL)

; builder is no more required, let's free a system resource
(g_object_unref builder)

; close button processor
(define quit (vm:pin (cons
   (cons gint (list GtkWidget* gpointer))
   (lambda (widget userdata)
      (print "Close pressed. Bye-bye.")
      (gtk_main_quit))
)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; show window and run
(gtk_main)
