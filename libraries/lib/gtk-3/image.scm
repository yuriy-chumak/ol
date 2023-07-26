(define-library (lib gtk-3 image)
   (export
      GtkImage*

      gtk_image_clear
      gtk_image_set_from_pixbuf
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gdk-3)
      (lib gtk-3 gtk))

(begin
   (define GtkImage* type-vptr)

   (define gtk_image_clear (GTK3 void "gtk_image_clear" GtkImage*))
   (define gtk_image_set_from_pixbuf (GTK3 void "gtk_image_set_from_pixbuf" GtkImage* GdkPixbuf*))

   (define (GtkImage props)
      ;...
      #false)
))
