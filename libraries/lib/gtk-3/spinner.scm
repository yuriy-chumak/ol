(define-library (lib gtk-3 spinner)
   (export
      GtkSpinner*

      gtk_spinner_start
      gtk_spinner_stop
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkSpinner* type-vptr)

   (define gtk_spinner_start (GTK3 void "gtk_spinner_start" GtkSpinner*))
   (define gtk_spinner_stop (GTK3 void "gtk_spinner_stop" GtkSpinner*))

   (define (GtkSpinner props)
      ;...
      #false)
))
