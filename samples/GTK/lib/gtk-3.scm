(define-library (lib gtk-3)
   (export
      GtkWidget*
      GtkApplication*
      GtkWindow*

      GApplicationFlags

      gtk_init
      gtk_main
      gtk_main_quit

      gtk_application_new
      gtk_application_window_new
      gtk_window_set_title
      gtk_window_set_default_size
      gtk_widget_show_all
      gtk_widget_show_all

      gtk_container_add

      ; todo: move next to (lib gtk bbox)
      gtk_button_box_new
      GTK_ORIENTATION_HORIZONTAL
      GTK_ORIENTATION_VERTICAL
      gtk_button_new_with_label

      ; todo: move to the (lib gtk container)
      GtkBuilder*
      gtk_builder_new
      gtk_builder_new_from_file
      gtk_builder_add_from_file
      gtk_builder_get_object

      gtk_builder_connect_signals

      ; todo: move to (lib gtk label)
      gtk_label_set_text
   )
   (import
      (scheme core)
      (otus ffi)
      (lib glib-2))

(begin

(define GtkWidget* type-vptr)
(define GtkApplication* type-vptr)
(define GApplicationFlags fft-int)
(define GtkWindow* type-vptr)
(define GtkCointainer* type-vptr)

(define GTK (load-dynamic-library "libgtk-3.so"))

(define gtk_init (GTK fft-void "gtk_init" fft-int& (fft& (fft* type-string))))
(define gtk_main (GTK fft-void "gtk_main"))
(define gtk_main_quit  (GTK fft-void "gtk_main_quit"))

(define gtk_application_new (GTK GtkApplication* "gtk_application_new" type-string GApplicationFlags))
(define gtk_application_window_new (GTK GtkWidget* "gtk_application_window_new" GtkApplication*))

(define gtk_window_set_title (GTK fft-void "gtk_window_set_title" GtkWindow* type-string))
(define gtk_window_set_default_size (GTK fft-void "gtk_window_set_default_size" GtkWindow* gint gint))

(define gtk_widget_show_all (GTK fft-void "gtk_widget_show_all" GtkWidget*))

(define gtk_container_add (GTK fft-void "gtk_container_add" GtkCointainer* GtkWidget*))

; (lib gtk builder)
(define GtkBuilder* type-vptr)
(define gtk_builder_new (GTK GtkBuilder* "gtk_builder_new"))
(define gtk_builder_new_from_file (GTK GtkBuilder* "gtk_builder_new_from_file" type-string))
(define gtk_builder_add_from_file (GTK guint "gtk_builder_add_from_file" GtkBuilder* gchar* (fft& GError*)))
(define gtk_builder_get_object (GTK GObject* "gtk_builder_get_object" GtkBuilder* type-string))

(define GtkBuilderConnectFunc type-callable) ; void (*GtkBuilderConnectFunc)(GtkBuilder *builder, GObject *object, const gchar *signal_name, const gchar *handler_name, GObject *connect_object, GConnectFlags flags, gpointer user_data)
(define gtk_builder_connect_signals (GTK fft-void "gtk_builder_connect_signals" GtkBuilder* GtkBuilderConnectFunc gpointer))

; (lib gtk bbox)
(define GtkOrientation fft-int) ; enum
(define GTK_ORIENTATION_HORIZONTAL 0)
(define GTK_ORIENTATION_VERTICAL 1)

(define gtk_button_box_new (GTK GtkWidget* "gtk_button_box_new" GtkOrientation))
(define gtk_button_new_with_label (GTK GtkWidget* "gtk_button_new_with_label" type-string))

; (lib gtk label)
(define GtkLabel* fft-void*)
(define gtk_label_set_text (GTK fft-void "gtk_label_set_text" GtkLabel* type-string))

))