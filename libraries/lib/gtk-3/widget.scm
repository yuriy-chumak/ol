(define-library (lib gtk-3 widget)
   (export
      GtkWidget*

      gtk_widget_show
      gtk_widget_show_all
      gtk_widget_hide
      gtk_widget_destroy
      gtk_widget_realize

      gtk_widget_is_sensitive
      gtk_widget_set_sensitive
      gtk_widget_get_sensitive

      gtk_widget_queue_resize
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkWidget* type-vptr)

   (define gtk_widget_show (GTK3 void "gtk_widget_show" GtkWidget*))
   (define gtk_widget_show_all (GTK3 void "gtk_widget_show_all" GtkWidget*))
   (define gtk_widget_hide (GTK3 void "gtk_widget_hide" GtkWidget*))
   (define gtk_widget_destroy (GTK3 void "gtk_widget_destroy" GtkWidget*))
   (define gtk_widget_realize (GTK3 void "gtk_widget_realize" GtkWidget*))

   (define gtk_widget_is_sensitive (GTK3 gboolean "gtk_widget_is_sensitive" GtkWidget*))  
   (define gtk_widget_set_sensitive (GTK3 void "gtk_widget_set_sensitive" GtkWidget* gboolean))
   (define gtk_widget_get_sensitive (GTK3 gboolean "gtk_widget_get_sensitive" GtkWidget*))

   (define gtk_widget_queue_resize (GTK3 void "gtk_widget_queue_resize" GtkWidget*))

))
