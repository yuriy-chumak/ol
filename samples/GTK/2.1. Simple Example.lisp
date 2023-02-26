#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3))

; create an application
(define app (gtk_application_new "org.gtk.example" G_APPLICATION_FLAGS_NONE))

; init:
(define activate (GTK_CALLBACK (app userdata)
   ; create an empty window
   (define window (gtk_application_window_new app))
   (gtk_window_set_title window "2.1 Simple Example")
   (gtk_window_set_default_size window 640 360)

   ; display the window
   (gtk_widget_show_all window)))

(g_signal_connect app "activate" (G_CALLBACK activate) NULL)

; run
(g_application_run app 0 #false)
