(define-library (lib gtk-3 box)
   (export
      GtkBox*

      gtk_box_pack_start
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkBox* type-vptr)

   (define gtk_box_pack_start (GTK3 GtkWidget* "gtk_box_pack_start" GtkBox* GtkWidget* gboolean gboolean guint))

   (define (GtkBox props)
      ;...
      #false)
))
