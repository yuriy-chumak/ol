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
   )
   (import
      (scheme core)
      (otus ffi)
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

   (define (GtkGlArea props)
      ;...
      #false)
))
