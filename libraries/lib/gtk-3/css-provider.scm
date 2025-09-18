(define-library (lib gtk-3 css-provider)
   (export
      GtkCssProvider*
      GTK_TYPE_CSS_PROVIDER
      gtk_css_provider_get_type

      gtk_css_provider_new
      gtk_css_provider_load_from_data
      gtk_css_provider_to_string
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkCssProvider* type-vptr)
   (define gtk_css_provider_get_type (GTK3 GType "gtk_css_provider_get_type"))
   (define GTK_TYPE_CSS_PROVIDER (gtk_css_provider_get_type))

   (define gtk_css_provider_new (GTK3 GtkCssProvider* "gtk_css_provider_new"))
   (define gtk_css_provider_load_from_data (GTK3 gboolean "gtk_css_provider_load_from_data" GtkCssProvider* gchar* gssize (fft& GError*)))

   (define :gtk_css_provider_to_string (GTK3 type-vptr "gtk_css_provider_to_string" GtkCssProvider*))
   (define (gtk_css_provider_to_string provider) ; todo: create universal method
      (define text-ptr (:gtk_css_provider_to_string provider))
      (define text (vptr->string text-ptr))
      (g_free text-ptr)
      text)
))
