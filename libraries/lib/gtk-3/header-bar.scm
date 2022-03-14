(define-library (lib gtk-3 header-bar)
   (export
      GtkHeaderBar*
      GTK_TYPE_HEADER_BAR
      gtk_header_bar_get_type

      gtk_header_bar_set_title
      gtk_header_bar_set_subtitle
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkHeaderBar* type-vptr)
   (define gtk_header_bar_get_type (GTK3 GType "gtk_header_bar_get_type"))
   (define GTK_TYPE_HEADER_BAR (gtk_header_bar_get_type))

   (define gtk_header_bar_set_title (GTK3 void "gtk_header_bar_set_title" GtkHeaderBar* gchar*))
   (define gtk_header_bar_set_subtitle (GTK3 void "gtk_header_bar_set_subtitle" GtkHeaderBar* gchar*))

))