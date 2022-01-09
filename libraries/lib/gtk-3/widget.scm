(define-library (lib gtk-3 widget)
   (export
      GtkWidget*

      gtk_widget_show_all
      gtk_widget_hide
      gtk_widget_destroy
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkWidget* type-vptr)

   (define gtk_widget_show_all (GTK3 fft-void "gtk_widget_show_all" GtkWidget*))
   (define gtk_widget_hide (GTK3 fft-void "gtk_widget_hide" GtkWidget*))
   (define gtk_widget_destroy (GTK3 fft-void "gtk_widget_destroy" GtkWidget*))

))
