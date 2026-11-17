#!/usr/bin/env ol
(import (Gtk 3.0))
(import (Gtk 3 GLArea))
(import
   (lib gtk-3 builder)
   (only (lib glib-2) g_error_get_message))

(import (OpenGL 1.0))

;; explicit init
(Gtk:init)

;; glade xml blueprint
(define blueprint (s/'/"/g "
<?xml version='1.0' encoding='UTF-8'?>
<!-- Generated with glade 3.40.0 -->
<interface>
  <requires lib='gtk+' version='3.20'/>
  <object class='GtkApplicationWindow' id='window'>
    <property name='can-focus'>False</property>
    <property name='default-width'>854</property>
    <property name='default-height'>480</property>
    <child>
      <object class='GtkGLArea' id='glarea'>
        <property name='visible'>True</property>
        <property name='app-paintable'>True</property>
        <property name='can-focus'>False</property>
      </object>
    </child>
  </object>
</interface>"))

;; load ui from the file
(define builder (GtkBuilder (gtk_builder_new_from_string blueprint -1)))

(define glarea (GtkGLArea ((builder 'get-object) "glarea") {
   'on-realize (lambda (this)
         ((this 'make-current))
         (define err ((this 'get-error)))
         (when err
            (runtime-error "Can't apply GL context"
                           (g_error_get_message err)))

         (print "OpenGL Vendor: " (glGetString GL_VENDOR))
         (print "OpenGL Version: " (glGetString GL_VERSION))
         (glClearColor 0.3 0.3 0.3 1.0))

   'on-render (lambda (this context)
         (glClear GL_COLOR_BUFFER_BIT)
         ((this 'queue-render)))
}))

;; setup main window
(define window ((builder 'get-Window) "window" {
   'title "1. Creating an OpenGL Window"
   'on-destroy (lambda (this)
      (Gtk:quit))
}))

;; display the window
((window 'show-all))

;; run
(Gtk:main)
