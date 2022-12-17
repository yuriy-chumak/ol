(define-library (lib gtk-3)
   (export
      GtkApplication*

      GApplicationFlags

      gtk_init
      gtk_main
      gtk_main_quit
      gtk_main_iteration
      gtk_events_pending

      gtk_check_version

      gtk_application_new
      gtk_application_window_new

      GtkContainer*
      gtk_container_add
      gtk_container_foreach

      ; todo: move next to (lib gtk bbox)
      gtk_button_box_new

      ; todo: move to (lib gtk glarea)
      GtkGLArea*
      GdkGLContext*
      gtk_gl_area_queue_render
      gtk_gl_area_make_current
      gtk_gl_area_get_error
      gtk_gl_area_set_required_version

      ; todo: move to (lib gtk adjustment)
      GtkAdjustment*
      gtk_adjustment_get_value
      gtk_adjustment_set_value

      ; todo: move to (lib gtk context)
      GtkStyleContext*
      gtk_widget_get_style_context
      gtk_widget_get_allocated_width
      gtk_widget_get_allocated_height
      gtk_render_background

      make-GtkTextIter
      GtkTextIter*

      ; 
      GtkTextView*

      ;
      GTK_STOCK_CANCEL
      GTK_RESPONSE_CANCEL
      GTK_STOCK_OPEN
      GTK_RESPONSE_ACCEPT

      (exports (lib gtk-3 gtk))

      (exports (lib gtk-3 widget))
      (exports (lib gtk-3 window))
      (exports (lib gtk-3 label))
      (exports (lib gtk-3 button))
      (exports (lib gtk-3 list-store))
      (exports (lib gtk-3 file-chooser))
      (exports (lib gtk-3 file-chooser-dialog))
      (exports (lib gtk-3 builder)))

   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)
      (lib cairo)

      (lib gtk-3 gtk)

      (lib gtk-3 widget)
      (lib gtk-3 window)
      (lib gtk-3 label)
      (lib gtk-3 button)
      (lib gtk-3 list-store)
      (lib gtk-3 file-chooser)
      (lib gtk-3 file-chooser-dialog)
      (lib gtk-3 builder))

(begin

   (setq guint fft-unsigned-int)

   (define GtkApplication* type-vptr)
   (define GApplicationFlags fft-int)
   (define GtkContainer* type-vptr)

   (define gtk_init (GTK3 fft-void "gtk_init" fft-int& (fft& (fft* type-string))))
   (define gtk_main (GTK3 fft-void "gtk_main"))
   (define gtk_main_quit  (GTK3 fft-void "gtk_main_quit"))
   (define gtk_main_iteration (GTK3 gboolean "gtk_main_iteration"))
   (define gtk_events_pending (GTK3 gboolean "gtk_events_pending"))

   (define gtk_check_version (GTK3 type-string "gtk_check_version" guint guint guint))

   (define gtk_application_new (GTK3 GtkApplication* "gtk_application_new" type-string GApplicationFlags))
   (define gtk_application_window_new (GTK3 GtkWidget* "gtk_application_window_new" GtkApplication*))

   (define gtk_container_add (GTK3 fft-void "gtk_container_add" GtkContainer* GtkWidget*))
   (define gtk_container_foreach (GTK3 fft-void "gtk_container_foreach" GtkContainer* GtkCallback gpointer))

   ; (lib gtk bbox)
   (define gtk_button_box_new (GTK3 GtkWidget* "gtk_button_box_new" GtkOrientation))

   ; (lib gtk glarea)
   (define GtkGLArea* fft-void*)
   (define GdkGLContext* fft-void*)
   (define gtk_gl_area_queue_render (GTK3 fft-void "gtk_gl_area_queue_render" GtkGLArea*))
   (define gtk_gl_area_make_current (GTK3 fft-void "gtk_gl_area_make_current" GtkGLArea*))
   (define gtk_gl_area_get_error (GTK3 GError* "gtk_gl_area_get_error" GtkGLArea*))
   (define gtk_gl_area_set_required_version (GTK3 GError* "gtk_gl_area_set_required_version" GtkGLArea* gint gint))

   ; (lib gtk adjustment)
   (define GtkAdjustment* fft-void*)
   (define gtk_adjustment_get_value (GTK3 gdouble "gtk_adjustment_get_value" GtkAdjustment*))
   (define gtk_adjustment_set_value (GTK3 fft-void "gtk_adjustment_set_value" GtkAdjustment* gdouble))

   ; (lib gtk context)
   (define GtkStyleContext* fft-void*)
   (define gtk_widget_get_style_context (GTK3 GtkStyleContext* "gtk_widget_get_style_context" GtkWidget*))
   (define gtk_widget_get_allocated_width (GTK3 fft-int "gtk_widget_get_allocated_width" GtkWidget*))
   (define gtk_widget_get_allocated_height (GTK3 fft-int "gtk_widget_get_allocated_height" GtkWidget*))
   (define gtk_render_background (GTK3 fft-void "gtk_render_background" GtkStyleContext* cairo_t* gdouble gdouble gdouble gdouble))

   ; (lib gtk textiter)
   (define |sizeof GtkTextIter| 80)
   (define (make-GtkTextIter)
      (make-bytevector |sizeof GtkTextIter| 0))
   (define GtkTextIter* fft-void*)

   (define GtkTextView* fft-void*)

   (define GTK_STOCK_CANCEL    "gtk-cancel")
   (define GTK_RESPONSE_CANCEL -6)
   (define GTK_STOCK_OPEN      "gtk-open")
   (define GTK_RESPONSE_ACCEPT -3)

))