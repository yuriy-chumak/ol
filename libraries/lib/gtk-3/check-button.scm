(define-library (lib gtk-3 check-button)
   (export
      GtkCheckButton*
      GTK_TYPE_COMBO_BOX
      gtk_combo_box_get_type

      gtk_combo_box_get_active_id
      gtk_combo_box_set_active_id
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkComboBox* type-vptr)
   (define gtk_combo_box_get_type (GTK3 GType "gtk_combo_box_get_type"))
   (define GTK_TYPE_COMBO_BOX (gtk_combo_box_get_type))

   (define gtk_combo_box_get_active_id (GTK3 type-string "gtk_combo_box_get_active_id" GtkComboBox*))
   (define gtk_combo_box_set_active_id (GTK3 gboolean "gtk_combo_box_set_active_id" GtkComboBox* type-string))

   (define (GtkComboBox props)
      ;...
      #false)
))
