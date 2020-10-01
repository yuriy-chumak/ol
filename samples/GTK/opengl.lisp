#!/usr/bin/ol

(import (otus ffi)
   (lib glib-2)
   (lib gdk-3)
   (lib gtk-3))
(import (otus random!))
(import (OpenGL version-3-2))

; main:
(gtk_init (box 0) #f)

; create window from file
(define builder (gtk_builder_new_from_file "opengl.glade"))
(gtk_builder_connect_signals builder #f NULL)

(define window (gtk_builder_get_object builder "window"))

(define glarea (gtk_builder_get_object builder "glarea"))
(define R (gtk_builder_get_object builder "R"))
(define G (gtk_builder_get_object builder "G"))
(define B (gtk_builder_get_object builder "B"))

(define randomizer (gtk_builder_get_object builder "randomizer"))

(g_object_unref builder)

; set initial random TGB values
(define (randomize!)
   (gtk_adjustment_set_value R (/ (rand! 256) 256))
   (gtk_adjustment_set_value G (/ (rand! 256) 256))
   (gtk_adjustment_set_value B (/ (rand! 256) 256)))
(randomize!) ; и немедленно выпил!

; randomizer button processor
(define randomize (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (randomize!)
      TRUE))))

(g_signal_connect randomizer "clicked" (G_CALLBACK randomize) NULL)

; close button processor:
(define quit (vm:pin (cons
   (list GtkWidget* gpointer)
   (lambda (widget userdata)
      (print "Close button pressed. Going out.")
      (gtk_main_quit)
))))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; renderer
(define render (vm:pin (cons
   (list GtkGLArea* GdkGLContext* gpointer)
   (lambda (widget context userdata)
      (glClearColor
         (gtk_adjustment_get_value R)
         (gtk_adjustment_get_value G)
         (gtk_adjustment_get_value B) 1)
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
         (print "OpenGL Vendor: " (glGetString GL_VENDOR))
         (print "OpenGL Version: " (glGetString GL_VERSION))
         (glClearColor (/ (rand! 255) 255) (/ (rand! 255) 255) (/ (rand! 255) 255) 0))
      TRUE
))))
(g_signal_connect glarea "realize" (G_CALLBACK realize) NULL)


(define resize (vm:pin (cons
   (list GtkGLArea* gint gint gpointer)
   (lambda (widget width height userdata)
      (print "info: resized to the " width " x " height)
      TRUE
))))
(g_signal_connect glarea "resize" (G_CALLBACK resize) NULL)


(define x (box 0))
(define idle (vm:pin (cons
   (list gpointer)
   (lambda (userdata)
      (print "idle: " (unbox x))
      (set-car! x (++ (unbox x)))
      TRUE))))
(gdk_threads_add_idle (G_CALLBACK idle) nullptr)


; show window and run
(gtk_widget_show_all window)
(gtk_main)
(vm:unpin quit)
