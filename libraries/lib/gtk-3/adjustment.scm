(define-library (lib gtk-3 adjustment)
   (description "A representation of an adjustable bounded value")
   (export
      GtkAdjustment*
      GTK_TYPE_ADJUSTMENT
      gtk_adjustment_get_type

      gtk_adjustment_new
      gtk_adjustment_get_value
      gtk_adjustment_set_value
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkAdjustment* type-vptr)
   (define gtk_adjustment_get_type (GTK3 GType "gtk_box_get_type"))
   (define GTK_TYPE_ADJUSTMENT (gtk_adjustment_get_type))

   (define gtk_adjustment_new (GTK3 GtkWidget* "gtk_adjustment_new" gdouble gdouble gdouble gdouble gdouble gdouble))
   ; gtk_adjustment_changed
   ; gtk_adjustment_value_changed
   ; gtk_adjustment_clamp_page
   (define gtk_adjustment_get_value (GTK3 gdouble "gtk_adjustment_get_value" GtkAdjustment*))
   (define gtk_adjustment_set_value (GTK3 fft-void "gtk_adjustment_set_value" GtkAdjustment* gdouble))
   ; gtk_adjustment_get_lower
   ; gtk_adjustment_set_lower
   ; gtk_adjustment_get_upper
   ; gtk_adjustment_set_upper
   ; gtk_adjustment_get_step_increment
   ; gtk_adjustment_set_step_increment
   ; gtk_adjustment_get_page_increment
   ; gtk_adjustment_set_page_increment
   ; gtk_adjustment_get_page_size
   ; gtk_adjustment_set_page_size
   ; gtk_adjustment_configure
   ; gtk_adjustment_get_minimum_increment

   (define (GtkAdjustment props)
      ;...
      #false)
))
