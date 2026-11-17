(define-library (Gtk 3 GLArea)
   (export
      GtkGLArea
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)

      (lib gtk-3 glarea))
      
(begin

   ; lisp interface
   (GTK_CLASS GLArea Widget {
         ; Marks the currently rendered data (if any) as invalid, and queues a redraw of the widget.
         'queue-render (lambda ()
            (gtk_gl_area_queue_render ptr))

         ; Retrieves the GdkGLContext used by area.
         'get-context (lambda ()
            (gtk_gl_area_get_context ptr))

         ; Ensures that the GdkGLContext used by area is associated with the GtkGLArea.
         'make-current (lambda ()
            (gtk_gl_area_make_current ptr))

         ; Gets the current error set on the area.
         'get-error (lambda ()
            (gtk_gl_area_get_error ptr))

         ; event handlers
         'set-realize-handler (GtkEventHandler "realize" ())
         'set-render-handler (GtkEventHandler "render" (context))
      }

      (
         ('on-realize . 'set-realize-handler)
         ('on-render . 'set-render-handler) )

   ; main
   (GTK_CLASS:CONSTRUCTORS
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ((ff? a1)
                  (make make (gtk_gl_area_new) a1))
               (else
                  (runtime-error "GtkGLArea: invalid argument" a1))))
   ))

))
