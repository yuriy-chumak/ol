(define-library (lib gtk-3 button)
   (export
      GtkButton*

      gtk_button_new_with_label
      gtk_button_get_label
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkButton* type-vptr)

   (define gtk_button_new_with_label (GTK3 GtkWidget* "gtk_button_new_with_label" type-string))
   (define gtk_button_get_label (GTK3 type-string "gtk_button_get_label" GtkButton*))

   (define (GtkButton props)
      ;...
      #false)
))
