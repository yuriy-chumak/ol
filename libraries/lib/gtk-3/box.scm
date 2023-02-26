(define-library (lib gtk-3 box)
   (description "A container box")
   (export
      GtkBox*
      GTK_TYPE_BOX
      gtk_box_get_type

      gtk_box_new
      gtk_box_pack_start
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkBox* type-vptr)
   (define gtk_box_get_type (GTK3 GType "gtk_box_get_type"))
   (define GTK_TYPE_BOX (gtk_box_get_type))

   (define gtk_box_new (GTK3 GtkWidget* "gtk_box_new" GtkOrientation gint))
   (define gtk_box_pack_start (GTK3 GtkWidget* "gtk_box_pack_start" GtkBox* GtkWidget* gboolean gboolean guint))
   (define gtk_box_pack_end (GTK3 void "gtk_box_pack_end" GtkBox* GtkWidget* gboolean gboolean guint))
   ; gtk_box_set_homogeneous
   ; gtk_box_get_homogeneous
   ; gtk_box_set_spacing
   ; gtk_box_get_spacing
   ; gtk_box_set_baseline_position
   ; gtk_box_get_baseline_position
   ; gtk_box_reorder_child
   ; gtk_box_query_child_packing
   ; gtk_box_set_child_packing
   ; gtk_box_set_center_widget
   ; gtk_box_get_center_widget

   (define (GtkBox props)
      ;...
      #false)
))
