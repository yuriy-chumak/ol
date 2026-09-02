#!/usr/bin/env ol
(import (Gtk 3.0))
(import (Gtk 3 GLArea))
(import (OpenGL 1.0))
(import (otus random!))

(import (Gtk 3.0 Gtk)) ; TEMP
(import (lib gtk-3 adjustment)) ; TEMP

;; explicit init
(Gtk:init)

;; load ui from the file
(define builder (GtkBuilder "X.0.GLArea.glade"))

(define R ((builder 'get-object) "R"))
(define G ((builder 'get-object) "G"))
(define B ((builder 'get-object) "B"))

(define glarea (GtkGLArea ((builder 'get-object) "glarea") {
   'on-realize (lambda (this)
         ((this 'make-current))
         (define err ((this 'get-error)))
         (when err
            (runtime-error "Can't apply GL context"
                  (g_error_get_message err)))

         (print "OpenGL Vendor: " (glGetString GL_VENDOR))
         (print "OpenGL Version: " (glGetString GL_VERSION))
         (glClearColor 0 0 0))

   'on-render (lambda (this context)
         (glClearColor
            (gtk_adjustment_get_value R)
            (gtk_adjustment_get_value G)
            (gtk_adjustment_get_value B) 1)
         (glClear GL_COLOR_BUFFER_BIT)
         ((this 'queue-render)))
}))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'on-destroy (lambda (this)
      (Gtk:quit))
}))

;; connect signals above
((builder 'connect-signals) {
   'randomize (GtkSignalHandler (this)
         (gtk_adjustment_set_value R (/ (rand! 256) 256))
         (gtk_adjustment_set_value G (/ (rand! 256) 256))
         (gtk_adjustment_set_value B (/ (rand! 256) 256)))
})

;; display the window
((window 'show-all))

;; run
(Gtk:main)
