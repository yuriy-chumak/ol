(define-library (lib gtk-3 bin)
   (description "A container with just one child")
   (export
      GtkBin*
      GTK_TYPE_BIN
      gtk_bin_get_type

      gtk_bin_get_child
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkBin* type-vptr)
   (define gtk_bin_get_type (GTK3 GType "gtk_bin_get_type"))
   (define GTK_TYPE_BIN (gtk_bin_get_type))

   (define gtk_bin_get_child (GTK3 GtkWidget* "gtk_bin_get_child" GtkBin*))
   ; gtk_bin_get_child
   ; _gtk_bin_set_child

   (define (GtkBin props)
      ;...
      #false)
))
