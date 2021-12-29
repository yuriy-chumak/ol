(define-library (lib gtk-3)
   (export
      GtkWidget*
      GtkApplication*
      GtkWindow*

      GApplicationFlags

      gtk_init
      gtk_main
      gtk_main_quit

      gtk_check_version

      gtk_application_new
      gtk_application_window_new
      gtk_window_set_title
      gtk_window_set_default_size
      gtk_widget_show_all
      gtk_widget_hide


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

      ; todo: move to (lib gtk label)
      gtk_label_get_text
      gtk_label_set_text

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

      GtkAboutDialog*
      gtk_dialog_run

   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2)
      (lib cairo))

(begin

   (setq guint fft-unsigned-int)

(define GtkWidget* type-vptr)
(define GtkApplication* type-vptr)
(define GApplicationFlags fft-int)
(define GtkWindow* type-vptr)
(define GtkContainer* type-vptr)
(define GtkCallback type-callable)

(define GTK (load-dynamic-library "libgtk-3.so"))

(define gtk_init (GTK fft-void "gtk_init" fft-int& (fft& (fft* type-string))))
(define gtk_main (GTK fft-void "gtk_main"))
(define gtk_main_quit  (GTK fft-void "gtk_main_quit"))

(define gtk_check_version (GTK type-string "gtk_check_version" guint guint guint))

(define gtk_application_new (GTK GtkApplication* "gtk_application_new" type-string GApplicationFlags))
(define gtk_application_window_new (GTK GtkWidget* "gtk_application_window_new" GtkApplication*))

(define gtk_window_set_title (GTK fft-void "gtk_window_set_title" GtkWindow* type-string))
(define gtk_window_set_default_size (GTK fft-void "gtk_window_set_default_size" GtkWindow* gint gint))

(define gtk_widget_show_all (GTK fft-void "gtk_widget_show_all" GtkWidget*))
(define gtk_widget_hide (GTK fft-void "gtk_widget_hide" GtkWidget*))

(define gtk_container_add (GTK fft-void "gtk_container_add" GtkContainer* GtkWidget*))
(define gtk_container_foreach (GTK fft-void "gtk_container_foreach" GtkContainer* GtkCallback gpointer))


; (lib gtk builder)
(define GtkBuilder* type-vptr)
(define gtk_builder_new (GTK GtkBuilder* "gtk_builder_new"))
(define gtk_builder_new_from_file (GTK GtkBuilder* "gtk_builder_new_from_file" type-string))
(define gtk_builder_add_from_file (GTK guint "gtk_builder_add_from_file" GtkBuilder* gchar* (fft& GError*)))
(define gtk_builder_get_object (GTK GObject* "gtk_builder_get_object" GtkBuilder* type-string))

(define GtkBuilderConnectFunc GtkCallback) ; void (*GtkBuilderConnectFunc)(GtkBuilder *builder, GObject *object, const gchar *signal_name, const gchar *handler_name, GObject *connect_object, GConnectFlags flags, gpointer user_data)
(define gtk_builder_add_callback_symbol (GTK fft-void "gtk_builder_add_callback_symbol" GtkBuilder* gchar* GCallback))
(define gtk_builder_connect_signals (GTK fft-void "gtk_builder_connect_signals" GtkBuilder* gpointer))

; (lib gtk bbox)
(define GtkButton* type-vptr)
(define GtkOrientation fft-int) ; enum
(define GTK_ORIENTATION_HORIZONTAL 0)
(define GTK_ORIENTATION_VERTICAL 1)

(define gtk_button_box_new (GTK GtkWidget* "gtk_button_box_new" GtkOrientation))
(define gtk_button_new_with_label (GTK GtkWidget* "gtk_button_new_with_label" type-string))
(define gtk_button_get_label (GTK type-string "gtk_button_get_label" GtkButton*))

; (lib gtk label)
(define GtkLabel* fft-void*)
(define gtk_label_get_text (GTK type-string "gtk_label_get_text" GtkLabel*))
(define gtk_label_set_text (GTK fft-void "gtk_label_set_text" GtkLabel* type-string))

; (lib gtk glarea)
(define GtkGLArea* fft-void*)
(define GdkGLContext* fft-void*)
(define gtk_gl_area_queue_render (GTK fft-void "gtk_gl_area_queue_render" GtkGLArea*))
(define gtk_gl_area_make_current (GTK fft-void "gtk_gl_area_make_current" GtkGLArea*))
(define gtk_gl_area_get_error (GTK GError* "gtk_gl_area_get_error" GtkGLArea*))
(define gtk_gl_area_set_required_version (GTK GError* "gtk_gl_area_set_required_version" GtkGLArea* gint gint))

; (lib gtk adjustment)
(define GtkAdjustment* fft-void*)
(define gtk_adjustment_get_value (GTK gdouble "gtk_adjustment_get_value" GtkAdjustment*))
(define gtk_adjustment_set_value (GTK fft-void "gtk_adjustment_set_value" GtkAdjustment* gdouble))

; (lib gtk context)
(define GtkStyleContext* fft-void*)
(define gtk_widget_get_style_context (GTK GtkStyleContext* "gtk_widget_get_style_context" GtkWidget*))
(define gtk_widget_get_allocated_width (GTK fft-int "gtk_widget_get_allocated_width" GtkWidget*))
(define gtk_widget_get_allocated_height (GTK fft-int "gtk_widget_get_allocated_height" GtkWidget*))
(define gtk_render_background (GTK fft-void "gtk_render_background" GtkStyleContext* cairo_t* gdouble gdouble gdouble gdouble))

; (lib gtk textiter)
(define |sizeof GtkTextIter| 80)
(define (make-GtkTextIter)
   (make-bytevector |sizeof GtkTextIter| 0))
(define GtkTextIter* fft-void*)

; (lib gtk textbuffer)
(define GtkTextBuffer* fft-void*)

(define gtk_text_buffer_insert_at_cursor (GTK fft-void "gtk_text_buffer_insert_at_cursor" GtkTextBuffer* type-string gint))
(define gtk_text_buffer_set_text (GTK fft-void "gtk_text_buffer_set_text" GtkTextBuffer* type-string gint))
(define gtk_text_buffer_get_text (GTK type-string "gtk_text_buffer_get_text" GtkTextBuffer* GtkTextIter* GtkTextIter* gboolean))

(define gtk_text_buffer_get_start_iter (GTK fft-void "gtk_text_buffer_get_start_iter" GtkTextBuffer* GtkTextIter*))
(define gtk_text_buffer_get_end_iter (GTK fft-void "gtk_text_buffer_get_end_iter" GtkTextBuffer* GtkTextIter*))

(define GtkTextView* fft-void*)

(define GtkAboutDialog* fft-void*)
(define gtk_dialog_run (GTK gint "gtk_dialog_run" GtkAboutDialog*))

))