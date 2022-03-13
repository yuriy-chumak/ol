(define-library (lib gtk-3)
   (export
      GtkApplication*

      GApplicationFlags

      gtk_init
      gtk_main
      gtk_main_quit

      gtk_check_version

      gtk_application_new
      gtk_application_window_new

      GtkContainer*
      gtk_container_add
      gtk_container_foreach

      ; todo: move next to (lib gtk bbox)
      gtk_button_box_new
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL
      gtk_button_new_with_label
      gtk_button_get_label

      ; todo: move to the (lib gtk container)
      GtkBuilder*
      gtk_builder_new
      gtk_builder_new_from_file
      gtk_builder_add_from_file
      gtk_builder_get_object

      gtk_builder_add_callback_symbol
      gtk_builder_connect_signals

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

      ; todo: move to (lib gtk textbuffer)
      ; https://developer.gnome.org/gtk3/stable/GtkTextBuffer.html
      GtkTextBuffer*
      gtk_text_buffer_insert_at_cursor
      gtk_text_buffer_set_text
      gtk_text_buffer_get_text

      gtk_text_buffer_get_start_iter
      gtk_text_buffer_get_end_iter

      ; 
      GtkTextView*

      ;
      GTK_STOCK_CANCEL
      GTK_RESPONSE_CANCEL
      GTK_STOCK_OPEN
      GTK_RESPONSE_ACCEPT

      (exports (lib gtk-3 widget))
      (exports (lib gtk-3 window))
      (exports (lib gtk-3 label))
      (exports (lib gtk-3 file-chooser))
      (exports (lib gtk-3 file-chooser-dialog))
      (exports (lib gtk-3 gtk)))
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)
      (lib cairo)

      (lib gtk-3 widget)
      (lib gtk-3 window)
      (lib gtk-3 label)
      (lib gtk-3 file-chooser)
      (lib gtk-3 file-chooser-dialog)

      (lib gtk-3 gtk))

(begin

   (setq guint fft-unsigned-int)

   (define GtkApplication* type-vptr)
   (define GApplicationFlags fft-int)
   (define GtkContainer* type-vptr)
   (define GtkCallback type-callable)

   (define gtk_init (GTK3 fft-void "gtk_init" fft-int& (fft& (fft* type-string))))
   (define gtk_main (GTK3 fft-void "gtk_main"))
   (define gtk_main_quit  (GTK3 fft-void "gtk_main_quit"))

   (define gtk_check_version (GTK3 type-string "gtk_check_version" guint guint guint))

   (define gtk_application_new (GTK3 GtkApplication* "gtk_application_new" type-string GApplicationFlags))
   (define gtk_application_window_new (GTK3 GtkWidget* "gtk_application_window_new" GtkApplication*))

   (define gtk_container_add (GTK3 fft-void "gtk_container_add" GtkContainer* GtkWidget*))
   (define gtk_container_foreach (GTK3 fft-void "gtk_container_foreach" GtkContainer* GtkCallback gpointer))


   ; (lib gtk builder)
   (define GtkBuilder* type-vptr)
   (define gtk_builder_new (GTK3 GtkBuilder* "gtk_builder_new"))
   (define gtk_builder_new_from_file (GTK3 GtkBuilder* "gtk_builder_new_from_file" type-string))
   (define gtk_builder_add_from_file (GTK3 guint "gtk_builder_add_from_file" GtkBuilder* gchar* (fft& GError*)))
   (define gtk_builder_get_object (GTK3 GObject* "gtk_builder_get_object" GtkBuilder* type-string))

   (define GtkBuilderConnectFunc GtkCallback) ; void (*GtkBuilderConnectFunc)(GtkBuilder *builder, GObject *object, const gchar *signal_name, const gchar *handler_name, GObject *connect_object, GConnectFlags flags, gpointer user_data)
   (define gtk_builder_add_callback_symbol (GTK3 fft-void "gtk_builder_add_callback_symbol" GtkBuilder* gchar* GCallback))
   (define gtk_builder_connect_signals (GTK3 fft-void "gtk_builder_connect_signals" GtkBuilder* gpointer))

   ; (lib gtk bbox)
   (define GtkButton* type-vptr)
   (define GtkOrientation fft-int) ; enum
   (define GTK_ORIENTATION_HORIZONTAL 0)
   (define GTK_ORIENTATION_VERTICAL 1)

   (define gtk_button_box_new (GTK3 GtkWidget* "gtk_button_box_new" GtkOrientation))
   (define gtk_button_new_with_label (GTK3 GtkWidget* "gtk_button_new_with_label" type-string))
   (define gtk_button_get_label (GTK3 type-string "gtk_button_get_label" GtkButton*))

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

   ; (lib gtk textbuffer)
   (define GtkTextBuffer* fft-void*)

   (define gtk_text_buffer_insert_at_cursor (GTK3 fft-void "gtk_text_buffer_insert_at_cursor" GtkTextBuffer* type-string gint))
   (define gtk_text_buffer_set_text (GTK3 fft-void "gtk_text_buffer_set_text" GtkTextBuffer* type-string gint))
   (define gtk_text_buffer_get_text (GTK3 type-string "gtk_text_buffer_get_text" GtkTextBuffer* GtkTextIter* GtkTextIter* gboolean))

   (define gtk_text_buffer_get_start_iter (GTK3 fft-void "gtk_text_buffer_get_start_iter" GtkTextBuffer* GtkTextIter*))
   (define gtk_text_buffer_get_end_iter (GTK3 fft-void "gtk_text_buffer_get_end_iter" GtkTextBuffer* GtkTextIter*))

   (define GtkTextView* fft-void*)

   (define GTK_STOCK_CANCEL    "gtk-cancel")
   (define GTK_RESPONSE_CANCEL -6)
   (define GTK_STOCK_OPEN      "gtk-open")
   (define GTK_RESPONSE_ACCEPT -3)

))