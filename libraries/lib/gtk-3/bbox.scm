(define-library (lib gtk-3 bbox)
   (description "A container for arranging buttons")
   (export
      GtkButtonBox*
      GTK_TYPE_BUTTON_BOX
      gtk_button_box_get_type

      gtk_button_box_new
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkButtonBox* type-vptr)
   (define gtk_button_box_get_type (GTK3 GType "gtk_button_box_get_type"))
   (define GTK_TYPE_BUTTON_BOX (gtk_button_box_get_type))
   
   (define gtk_button_box_new (GTK3 GtkWidget* "gtk_button_box_new" GtkOrientation))
   ; gtk_button_box_get_layout
   ; gtk_button_box_set_layout
   ; gtk_button_box_get_child_secondary
   ; gtk_button_box_set_child_secondary
   ; gtk_button_box_get_child_non_homogeneous
   ; gtk_button_box_set_child_non_homogeneous

   (define (GtkButtonBox props)
      ;...
      #false)
))
