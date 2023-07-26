(define-library (lib gtk-3 label)
   (export
      GtkLabel*

      gtk_label_get_text
      gtk_label_set_text

      gtk_label_set_markup
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkLabel* type-vptr)

   (define gtk_label_get_text (GTK3 type-string "gtk_label_get_text" GtkLabel*))
   (define gtk_label_set_text (GTK3 void "gtk_label_set_text" GtkLabel* type-string))

   (define gtk_label_set_markup (GTK3 void "gtk_label_set_markup" GtkLabel* type-string))

   (define (GtkLabel props)
      ;...
      #false)
))
