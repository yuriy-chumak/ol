(define-library (lib gtk-3 style-provider)
   (export
      GtkStyleProvider*
      GTK_TYPE_STYLE_PROVIDER
      gtk_style_provider_get_type

      ;; ...
      GTK_STYLE_PROVIDER_PRIORITY_APPLICATION
      ;; ...

   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkStyleProvider* type-vptr)
   (define gtk_style_provider_get_type (GTK3 GType "gtk_style_provider_get_type"))
   (define GTK_TYPE_STYLE_PROVIDER (gtk_style_provider_get_type))

   (define GTK_STYLE_PROVIDER_PRIORITY_APPLICATION 600)

))
