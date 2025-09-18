(define-library (lib gtk-3 label)
   (export
      GtkLabel*
      GTK_TYPE_LABEL
      gtk_label_get_type

      gtk_label_new
      gtk_label_get_text
      gtk_label_set_text
      gtk_label_set_markup
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkLabel* type-vptr)
   (define gtk_label_get_type (GTK3 GType "gtk_label_get_type"))
   (define GTK_TYPE_LABEL (gtk_label_get_type))

   (define gtk_label_new (GTK3 GtkWidget* "gtk_label_new" type-string))
   (define gtk_label_get_text (GTK3 type-string "gtk_label_get_text" GtkLabel*))
   (define gtk_label_set_text (GTK3 void "gtk_label_set_text" GtkLabel* type-string))

   (define gtk_label_set_markup (GTK3 void "gtk_label_set_markup" GtkLabel* type-string))

))
