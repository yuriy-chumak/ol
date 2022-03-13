(define-library (lib gtk-3 window)
   (export
      GtkWindow*

      gtk_window_set_title
      gtk_window_set_default_size

      gtk_window_present
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkWindow* type-vptr)

   (define gtk_window_set_title (GTK3 void "gtk_window_set_title" GtkWindow* type-string))
   (define gtk_window_set_default_size (GTK3 void "gtk_window_set_default_size" GtkWindow* gint gint))

   (define gtk_window_present (GTK3 void "gtk_window_present" GtkWindow*))
))
