(define-library (lib gtk-3 level-bar)
   (description "A widget that can be used as a level indicator.")
   (export
      GtkLevelBar*
      GTK_TYPE_LEVEL_BAR
      gtk_level_bar_get_type

      gtk_level_bar_set_value
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkLevelBar* type-vptr)
   (define gtk_level_bar_get_type (GTK3 GType "gtk_level_bar_get_type"))
   (define GTK_TYPE_LEVEL_BAR (gtk_level_bar_get_type))

   (define gtk_level_bar_set_value (GTK3 void "gtk_level_bar_set_value" GtkLevelBar* gdouble))

   (define (GtkLevelBar props)
      ;...
      #false)
))
