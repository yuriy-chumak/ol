(define-library (lib gtk-3 window)
   (export
      GtkWindow*

      gtk_window_set_title
      gtk_window_set_default_size
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkWindow* type-vptr)

   (define gtk_window_set_title (GTK3 fft-void "gtk_window_set_title" GtkWindow* type-string))
   (define gtk_window_set_default_size (GTK3 fft-void "gtk_window_set_default_size" GtkWindow* gint gint))

))
