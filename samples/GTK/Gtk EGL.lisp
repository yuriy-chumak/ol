#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gdk-3)
   (lib gtk-3))
(import (EGL 1.4))
(import (OpenGL 1.0))

; create an application
(define app (gtk_application_new "org.gtk.example" G_APPLICATION_FLAGS_NONE))

(import (scheme dynamic-bindings))
(define context (make-parameter [#f #f #f #f]))
(define realize
   (GTK_CALLBACK (widget)
      (define egl_config (vm:cast 0 EGLConfig))
      (define n_config '(0))
      (define attributes [EGL_RENDERABLE_TYPE EGL_OPENGL_BIT EGL_NONE])

      (define egl_display (eglGetDisplay (gdk_x11_display_get_xdisplay (gtk_widget_get_display widget))))
      (eglInitialize egl_display #f #f)
      (eglChooseConfig egl_display attributes egl_config 1 n_config)
      (eglBindAPI EGL_OPENGL_API)

      (define egl_surface (eglCreateWindowSurface egl_display egl_config (gdk_x11_window_get_xid (gtk_widget_get_window widget)) #f))
      (define egl_context (eglCreateContext egl_display egl_config EGL_NO_CONTEXT #f))

      (context [egl_display egl_surface egl_context])
   ))

(define draw
   (GTK_CALLBACK (widget)
      (vector-apply (context) (lambda (egl_display egl_surface egl_context)
         (eglMakeCurrent egl_display egl_surface egl_surface egl_context)

         (glViewport 0 0 (gtk_widget_get_allocated_width widget) (gtk_widget_get_allocated_height widget))
         (glClearColor 0 0 0 1)
         (glClear (bor GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))

         (glLoadIdentity)
         (glOrtho 0 100  0 100  0 1)

         (glBegin GL_TRIANGLES)
            (glColor3f 1 0 0)
            (glVertex2f 50 10)
            (glColor3f 0 1 0)
            (glVertex2f 90 90)
            (glColor3f 0 0 1)
            (glVertex2f 10 90)
         (glEnd)

         (eglSwapBuffers egl_display egl_surface)
         TRUE))
   ))

; init:
(define activate (GTK_CALLBACK (app userdata)
   (define window (gtk_application_window_new app))
   (gtk_widget_set_double_buffered window FALSE)
   (gtk_window_set_title window "GLArea with OpenGL 2.1")
   (gtk_window_set_default_size window 640 360)

   (g_signal_connect window "realize" (G_CALLBACK realize) NULL)
   (g_signal_connect window "draw" (G_CALLBACK draw) NULL)

   ; display the window
   (gtk_widget_show_all window)))

(g_signal_connect app "activate" (G_CALLBACK activate) NULL)

; run
(g_application_run app 0 #false)
