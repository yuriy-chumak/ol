#!/usr/bin/ol

(import (otus ffi)
   (lib glib-2)
   (lib gtk-3))

(define print_hello (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      ;(print "clicked " (bytes->string (string->list (gtk_button_get_label widget)))) ; utf-8 conversion

      (let ((str (gtk_button_get_label widget)))
         (gtk_text_buffer_insert_at_cursor userdata str (string-length str)))
      TRUE
))))

; main:
(gtk_init (box 0) #f)

; create window from file
(define builder (gtk_builder_new_from_file "ithkuil.glade"))
(gtk_builder_connect_signals builder #f NULL)

(define window (gtk_builder_get_object builder "window"))
(gtk_widget_show_all window)

(define textbuffer (gtk_builder_get_object builder "textbuffer"))

(define hello-callback (G_CALLBACK print_hello))
(define set-clicked-signal (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (g_signal_connect widget "clicked" hello-callback userdata)
      TRUE
))))
(define callback (G_CALLBACK set-clicked-signal))
(define textbuffer (gtk_builder_get_object builder "textbuffer"))

(for-each (lambda (name)
      (define container (gtk_builder_get_object builder name))
      (gtk_container_foreach container callback textbuffer))
   '("alphabet" "numpad"))

(g_object_unref builder)

; close button processor:
(define quit (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (print "Close button pressed. Going out.")
      (gtk_main_quit)))))

(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; show window and run
(gtk_main)
(vm:unpin quit)
