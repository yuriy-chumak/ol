(define-library (lib gtk-3 container)
   (description "Base class for widgets which contain other widgets")
   (export
      GtkContainer*
      GTK_TYPE_CONTAINER
      gtk_container_get_type

      gtk_container_remove
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkContainer* type-vptr)
   (define gtk_container_get_type (GTK3 GType "gtk_container_get_type"))
   (define GTK_TYPE_CONTAINER (gtk_container_get_type))

   (define gtk_container_add (GTK3 void "gtk_container_add" GtkContainer* GtkWidget*))
   (define gtk_container_remove (GTK3 void "gtk_container_remove" GtkContainer* GtkWidget*))

   (define (GtkContainer props)
      ;...
      #false)
))
