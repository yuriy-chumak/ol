#!/usr/bin/env ol
(import (lib glib-2)
   (lib gtk-3)
   (lib gtk-3 glarea))
(import (lib gl config))

(import (OpenGL 1.0))

; create an application
(define app (gtk_application_new "org.gtk.example" G_APPLICATION_FLAGS_NONE))

; init:
(define activate (GTK_CALLBACK (app userdata)
   ; create an empty window
   (define window (gtk_application_window_new app))
   (gtk_window_set_title window "1. Creating an OpenGL Window")
   (gtk_window_set_default_size window (config 'width 640) (config 'height 360))

   ; create an glarea
   (define glarea (gtk_gl_area_new))
   (gtk_container_add window glarea)
   (define realize (GTK_CALLBACK (this)
      (gtk_gl_area_make_current this)
      (define err (gtk_gl_area_get_error this))
      (when err
         (runtime-error "Can't apply GL context"
                        (g_error_get_message err)))

      (print "OpenGL Vendor: " (glGetString GL_VENDOR))
      (print "OpenGL Version: " (glGetString GL_VERSION))
      (glClearColor 0.3 0.3 0.3 1.0)))
   (g_signal_connect glarea "realize" (G_CALLBACK realize) NULL)

   (define render (GTK_CALLBACK (this context)
      (glClear GL_COLOR_BUFFER_BIT)
      (gtk_gl_area_queue_render this)))
   (g_signal_connect glarea "render" (G_CALLBACK render) NULL)


   ; display the window
   (gtk_widget_show_all window)))

(g_signal_connect app "activate" (G_CALLBACK activate) NULL)

; run
(g_application_run app 0 #false)
(g_object_unref app)
