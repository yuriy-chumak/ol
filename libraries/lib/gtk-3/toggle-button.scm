(define-library (lib gtk-3 toggle-button)
   (export
      GtkToggleButton*

      gtk_toggle_button_get_active
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkToggleButton* type-vptr)

   (define gtk_toggle_button_get_active (GTK3 gboolean "gtk_toggle_button_get_active" GtkToggleButton*))

))
