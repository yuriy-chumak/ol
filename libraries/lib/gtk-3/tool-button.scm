(define-library (lib gtk-3 tool-button)
   (export
      GtkToolButton*
      GTK_TYPE_TOOL_BUTTON
      gtk_tool_button_get_type

      gtk_tool_button_set_stock_id
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkToolButton* type-vptr)
   (define gtk_tool_button_get_type (GTK3 GType "gtk_tool_button_get_type"))
   (define GTK_TYPE_TOOL_BUTTON (gtk_tool_button_get_type))
   
   (define gtk_tool_button_set_stock_id (GTK3 void "gtk_tool_button_set_stock_id" GtkToolButton* gchar*))

   (define (GtkToolButton props)
      ;...
      #false)
))
