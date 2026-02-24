#!/usr/bin/env ol

(import
   (lib glib-2)
   (lib gdk-3)
   (lib gtk-3))
(import (lib gtk-3 glarea))
(import (otus random!))
(import (OpenGL 3.0))

; main:
(gtk_init (box 0) #f)

; create window from ui template file
(define builder (gtk_builder_new_from_string (file->string "./ui.glade") -1))
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
(randomize!) ; и немедленно выпил

; "randomizer" button pressed
(define randomize
   (GTK_CALLBACK (widget userdata)
      (randomize!)
      TRUE))
(g_signal_connect randomizer "clicked" (G_CALLBACK randomize) NULL)

; close button processor:
(define quit (vm:pin (cons
   (list fft-int GtkWidget* gpointer)
   (lambda (widget userdata)
      (print "Close button pressed. Going out...")
      (gtk_main_quit)
))))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; gl init
(define GLP (box 0))
(define VAO (box 0))

(define realize
   (GTK_CALLBACK (widget userdata)
      (gtk_gl_area_make_current widget)
      (define err (gtk_gl_area_get_error widget))
      (if err
      then
         (print "gl error")
         (g_error_free err)
      else
         ; opengl info
         (print "OpenGL Vendor: " (glGetString GL_VENDOR))
         (print "OpenGL Version: " (glGetString GL_VERSION))

         (glShadeModel GL_SMOOTH)

         ; vertices
         (glGenVertexArrays 1 VAO)
         (glBindVertexArray (unbox VAO))

         (define VBO (box 0))
         (glGenBuffers 1 VBO)
         (glBindBuffer GL_ARRAY_BUFFER (unbox VBO))
         (define Vertices '((-0.8 -0.7 0)
                            ( 0.8 -0.7 0)
                            (-0.8  0.7 0)
                            ( 0.8  0.7 0)))
         (glBufferData GL_ARRAY_BUFFER (* (sizeof fft-float) (length Vertices) 3)
            (cons (fft* fft-float) (apply append Vertices)) GL_STATIC_DRAW)

         (glEnableVertexAttribArray 0)
         (glVertexAttribPointer 0 3 GL_FLOAT GL_FALSE 0 #f)
         (glBindVertexArray 0)

         ; shader
         (define po (gl:create-program
         "#version 330 core
            in vec4 position;
            void main() {
               gl_Position = position;
            }"
         "#version 330 core
            uniform float time;
            uniform vec2 dimensions;

            void main(void) {
               vec2  p = 7.*(2.*gl_FragCoord.xy-dimensions.xy)/dimensions.y;
               float m1 = sin(length(p)*0.3-time*0.3);
               float m2 = sin(0.3*(length(p)*0.3-time*0.3));
               float c1 = 0.012/abs(length(mod(p,2.0*m1)-m1)-0.3);
               float c2 = 0.012/abs(length(mod(p,2.0*m2)-m2)-0.3);
               gl_FragColor = vec4(vec3(1.,2.,8.)*c1+vec3(8.,2.,1.)*c2, 1.);
            }"))
         (set-car! GLP po))
      TRUE
))
(g_signal_connect glarea "realize" (G_CALLBACK realize) NULL)

; gl render
(define render
   (GTK_CALLBACK (widget context userdata)
      (glClearColor
         (gtk_adjustment_get_value R)
         (gtk_adjustment_get_value G)
         (gtk_adjustment_get_value B) 1)
      (glClear GL_COLOR_BUFFER_BIT)

      (define po (unbox GLP))
      (glUseProgram po)
      (glUniform1f (glGetUniformLocation po "time") (/ (mod (clock-ms) 1000000) #i1000))
      (glUniform2f (glGetUniformLocation po "dimensions")
         (gtk_widget_get_allocated_width widget) (gtk_widget_get_allocated_height widget))

      (glBindVertexArray (unbox VAO))
      (glDrawArrays GL_TRIANGLE_STRIP 0 4)
      (glBindVertexArray 0)

      (glUseProgram 0)

      (gtk_gl_area_queue_render widget) ; immediately rerender
      TRUE
))
(g_signal_connect glarea "render" (G_CALLBACK render) NULL)

(define resize (vm:pin (cons
   (list fft-int GtkGLArea* gint gint gpointer)
   (lambda (widget width height userdata)
      (print "info: resized to the " width " x " height)
      TRUE
))))
(g_signal_connect glarea "resize" (G_CALLBACK resize) NULL)

; multithreading support callback:
(define idle (GTK_CALLBACK (userdata)
   (sleep 0) TRUE))
(gdk_threads_add_idle (G_CALLBACK idle) nullptr)

; show window and run
(gtk_widget_show_all window)
(gtk_main)

(print "bye-bye.")