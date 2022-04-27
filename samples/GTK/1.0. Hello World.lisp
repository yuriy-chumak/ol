#!/usr/bin/env ol
(import
      (lib glib-2)
      (lib gtk-3))

; hello
(define print_hello (GTK_CALLBACK (widget userdata)
   (print "Hello World")))

; init
(define activate (GTK_CALLBACK (app userdata)
   ; create an empty window
   (define window (gtk_application_window_new app))
   (gtk_window_set_title window "Window")
   (gtk_window_set_default_size window 200 40)

   ; the button
   (define button (gtk_button_new_with_label "Press Me"))
   (g_signal_connect button "clicked" (G_CALLBACK print_hello) NULL)
   (gtk_container_add window button)

   ; display the window
   (gtk_widget_show_all window)))

; main
; create an application
(define app (gtk_application_new "org.gtk.example" G_APPLICATION_FLAGS_NONE))
(g_signal_connect app "activate" (G_CALLBACK activate) NULL)

; run
; note: (command-line) will not be changed by g_application_run call
(define status (g_application_run app (length (command-line)) (command-line)))
(g_object_unref app)

status ; just indirectly return result
