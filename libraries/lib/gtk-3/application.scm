(define-library (lib gtk-3 application)
   (description "Application class")
   (export
      GtkApplication*

      gtk_application_new
      gtk_application_window_new
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkApplication* type-vptr)
   (define GApplicationFlags fft-int)

   (define gtk_application_new (GTK3 GtkApplication* "gtk_application_new" type-string GApplicationFlags))
   (define gtk_application_window_new (GTK3 GtkWidget* "gtk_application_window_new" GtkApplication*))

))
