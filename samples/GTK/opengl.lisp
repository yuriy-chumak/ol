#!/usr/bin/ol

(import (otus ffi)
   (lib glib-2)
   (lib gtk-3))
(import (otus random!))
(import (OpenGL version-3-2))

; main:
(gtk_init (box 0) #f)

; create window from file
(define builder (gtk_builder_new_from_file (c-string "opengl.glade")))
(gtk_builder_connect_signals builder #f NULL)

(define window (gtk_builder_get_object builder (c-string "window")))

(define glarea (gtk_builder_get_object builder (c-string "glarea")))
(gtk_gl_area_set_required_version glarea 3 2)

(g_object_unref builder)

; close button processor:
(define quit (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (print "Close button pressed. Going out.")
      (gtk_main_quit)))))

(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; renderer
(define render (vm:pin (cons
   (list GtkGLArea* GdkGLContext* gpointer)
   (lambda (widget context userdata)
      (glClear GL_COLOR_BUFFER_BIT)
      (gtk_gl_area_queue_render widget)
      TRUE
))))
(g_signal_connect glarea "render" (G_CALLBACK render) NULL)

; initializer
(define realize (vm:pin (cons
   (list GtkGLArea* gpointer)
   (lambda (widget userdata)
      (gtk_gl_area_make_current widget)
      (define error (gtk_gl_area_get_error widget))
      (when error
         (print "error")
         (g_error_free error))
      (unless error
         (print "Vendor: " (glGetString GL_VENDOR))
         (print "Version: " (glGetString GL_VERSION))
         (glClearColor (/ (rand! 255) 255) (/ (rand! 255) 255) (/ (rand! 255) 255) 0))
      TRUE
))))
(g_signal_connect glarea "realize" (G_CALLBACK realize) NULL)


(define resize (vm:pin (cons
   (list GtkGLArea* gint gint gpointer)
   (lambda (widget width height userdata)
      (print "resized to the " width " x " height)
      TRUE
))))
(g_signal_connect glarea "resize" (G_CALLBACK resize) NULL)


; show window and run
(gtk_widget_show_all window)
(gtk_main)
(vm:unpin quit)
