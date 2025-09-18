(define-library (lib gtk-3 button)
   (export
      ; c types
      GtkButton*
      ; c interface
      gtk_button_new
      gtk_button_new_with_label
      gtk_button_set_label
      gtk_button_get_label
      gtk_button_set_image
      gtk_button_set_use_stock
      gtk_button_set_always_show_image
      gtk_button_set_relief

      GTK_RELIEF_NORMAL
      GTK_RELIEF_HALF
      GTK_RELIEF_NONE
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
   (define GtkReliefStyle gint)

   (define GTK_RELIEF_NORMAL 0)
   (define GTK_RELIEF_HALF   1)
   (define GTK_RELIEF_NONE   2)

   (define gtk_button_new (GTK3 GtkWidget* "gtk_button_new"))
   (define gtk_button_new_with_label (GTK3 GtkWidget* "gtk_button_new_with_label" gchar*))
   (define gtk_button_get_label (GTK3 type-string "gtk_button_get_label" GtkButton*))
   (define gtk_button_set_label (GTK3 void "gtk_button_set_label" GtkButton* gchar*))
   (define gtk_button_set_image (GTK3 void "gtk_button_set_image" GtkButton* GtkWidget*))
   (define gtk_button_set_use_stock (GTK3 void "gtk_button_set_use_stock" GtkButton* gboolean))
   (define gtk_button_set_always_show_image (GTK3 void "gtk_button_set_always_show_image" GtkButton* gboolean))
   (define gtk_button_set_relief (GTK3 void "gtk_button_set_relief" GtkButton* GtkReliefStyle))

))
