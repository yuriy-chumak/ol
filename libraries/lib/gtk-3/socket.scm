(define-library (lib gtk-3 socket)
   (export
      GtkSocket*

      gtk_socket_new
      gtk_socket_get_id
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkSocket* type-vptr)
   (define Window type-vptr)

   (define gtk_socket_new (GTK3 GtkWidget* "gtk_socket_new"))
   (define gtk_socket_get_id (GTK3 fft-unsigned-long "gtk_socket_get_id" GtkSocket*))

   (define (GtkSocket props)
      ;...
      #false)
))
