#!/usr/bin/env ol
(import
      (lib glib-2)
      (lib gtk-3)
      (lib gtk-3 message-dialog))
(import (only (otus syscall) strftime))

; just check a gtk version (not required)
(define notify (gtk_check_version 3 0 0))
(when notify
   (print "error: " notify)
   (exit 1))

; init a library
(gtk_init '(0) #f)

; create an application
(define app (gtk_application_new "org.gtk.example" G_APPLICATION_FLAGS_NONE))

; button handler
(define clicked
   (GTK_CALLBACK (widget userdata)
      (define dialog (gtk_message_dialog_new #f
         GTK_DIALOG_DESTROY_WITH_PARENT
         GTK_MESSAGE_INFO GTK_BUTTONS_CLOSE
         (string-append "Button clicked at " (strftime "%F %H:%M:%S\0"))))
      (gtk_dialog_run dialog)
      (gtk_widget_destroy dialog)
      TRUE))

; close processor
(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      (g_application_quit app)
      TRUE))

; init
(define activate
   (GTK_CALLBACK (app userdata)
      ; create a window
      (define window (gtk_application_window_new app))
      (gtk_window_set_title window "2.2. Extended Example")
      (gtk_window_set_default_size window 640 360)

      ; create a button
      (define button_box (gtk_button_box_new GTK_ORIENTATION_HORIZONTAL))
      (gtk_container_add window button_box)

      (define button (gtk_button_new_with_label "Click me"))
      (g_signal_connect button "clicked"  (G_CALLBACK clicked) NULL)
      (gtk_container_add button_box button)

      ; subscribe to "close" window button
      (g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

      ; display the window
      (gtk_widget_show_all window)))

(g_signal_connect app "activate" (G_CALLBACK activate) NULL)

; run
(g_application_run app 0 #false)

; cleanup (is not mandatory but appreciated)
(vm:unpin activate)
(vm:unpin quit)
(g_object_unref app)
