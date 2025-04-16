(define-library (lib gtk-3 button)
   (export
      ; c types
      GtkButton*
      ; c interface
      gtk_button_new
      gtk_button_new_with_label
      gtk_button_set_label
      gtk_button_get_label
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (owl string)
      (lib gtk-3 gtk)
      (lib gtk-3 widget)
      (lib gtk-3 bin)
      (lib gtk-3 label))

(begin
   (define GtkButton* type-vptr)

   (define gtk_button_new (GTK3 GtkWidget* "gtk_button_new"))
   (define gtk_button_new_with_label (GTK3 GtkWidget* "gtk_button_new_with_label" type-string))
   (define gtk_button_get_label (GTK3 type-string "gtk_button_get_label" GtkButton*))
   (define gtk_button_set_label (GTK3 void "gtk_button_set_label" GtkButton* type-string))

))
