#!/usr/bin/ol

(import (otus ffi)
   (lib glib-2)
   (lib gtk-3))

; let's read config:
(import (file json)
   (otus syscall))

(define config (or
   (read-json-file "config.json")
   { ;default config:
      'timestamp (strftime (c-string "%F %x:%S"))
   }))

(print config)


(define print_hello (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (gtk_label_set_text userdata (c-string (strftime (c-string "%F %x:%S"))))
      TRUE
))))

; main:
(gtk_init (box 0) #f)

; create window from file
(define builder (gtk_builder_new_from_file (c-string "bookmarks.glade")))
(gtk_builder_connect_signals builder #f NULL)

(define window (gtk_builder_get_object builder (c-string "window")))
(gtk_widget_show_all window)



(define something (gtk_builder_get_object builder (c-string "something")))
(print "something: " something)

(define label (gtk_builder_get_object builder (c-string "label1")))

(g_signal_connect something (c-string "clicked") (G_CALLBACK print_hello) label)


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
