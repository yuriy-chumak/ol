(define-library (lib gtk-3 glarea)
   (description "A widget for custom drawing with OpenGL")
   (export
      GtkGLArea*
      GTK_TYPE_GL_AREA
      gtk_gl_area_get_type

      gtk_gl_area_new
      gtk_gl_area_queue_render
      gtk_gl_area_make_current
      gtk_gl_area_get_context
      gtk_gl_area_set_required_version

      gtk_gl_area_get_error

      ; lisp interface
      GtkGLArea
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib gdk-3)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkGLArea* type-vptr)
   (define GdkGLContext* type-vptr)
   (define gtk_gl_area_get_type (GTK3 GType "gtk_gl_area_get_type"))
   (define GTK_TYPE_GL_AREA (gtk_gl_area_get_type))
   
   (define gtk_gl_area_new (GTK3 GtkWidget* "gtk_gl_area_new"))
   ; gtk_gl_area_set_use_es
   ; gtk_gl_area_get_use_es
   (define gtk_gl_area_set_required_version (GTK3 GError* "gtk_gl_area_set_required_version" GtkGLArea* gint gint))
   ; gtk_gl_area_get_required_version
   ; gtk_gl_area_get_has_alpha
   ; gtk_gl_area_set_has_alpha
   ; gtk_gl_area_get_has_depth_buffer
   ; gtk_gl_area_set_has_depth_buffer
   ; gtk_gl_area_get_has_stencil_buffer
   ; gtk_gl_area_set_has_stencil_buffer
   ; gtk_gl_area_get_auto_render
   ; gtk_gl_area_set_auto_render
   (define gtk_gl_area_queue_render (GTK3 fft-void "gtk_gl_area_queue_render" GtkGLArea*))
   (define gtk_gl_area_get_context (GTK3 GdkGLContext* "gtk_gl_area_get_context" GtkGLArea*))
   (define gtk_gl_area_make_current (GTK3 fft-void "gtk_gl_area_make_current" GtkGLArea*))
   ; gtk_gl_area_attach_buffers
   ; gtk_gl_area_set_error
   (define gtk_gl_area_get_error (GTK3 GError* "gtk_gl_area_get_error" GtkGLArea*))

   ; lisp interface
   (define GtkGLArea
      (define (make ptr options)
         (define base (GtkWidget ptr options))
         (define this (ff-replace base {
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

            'set-realize-handler (GtkEventHandler "realize" (widget userdata)
                     (make widget #e))
            'set-render-handler (GtkEventHandler "render" (widget context userdata)
                     (make widget #e))

            ; internals
            'super base
         }))
         ; apply options
         (if (options 'on-realize #f)
            ((this 'set-realize-handler) (options 'on-realize)))
         (if (options 'on-render #f)
            ((this 'set-render-handler) (options 'on-render)))
         ; smart object
         (GObject this))

   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               ((GObject? a1)
                  (make (gtk_gl_area_new) #e))
               (else
                  (runtime-error "GtkGLArea: invalid argument" a1)) ))
      ((a1 op) (cond
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkGLArea: invalid arguments" (cons a1 op))) ))
   ))
))
