(define-library (lib gtk-3 style-context)
   (description "Rendering UI elements")
   (export
      GtkStyleContext*
      GTK_TYPE_STYLE_CONTEXT
      gtk_style_context_get_type

      ;; GTK_STYLE_PROPERTY_BACKGROUND_COLOR
      ;; GTK_STYLE_PROPERTY_COLOR
      ;; GTK_STYLE_PROPERTY_FONT
      ;; GTK_STYLE_PROPERTY_PADDING
      ;; GTK_STYLE_PROPERTY_BORDER_WIDTH
      ;; GTK_STYLE_PROPERTY_MARGIN
      ;; GTK_STYLE_PROPERTY_BORDER_RADIUS
      ;; GTK_STYLE_PROPERTY_BORDER_STYLE
      ;; GTK_STYLE_PROPERTY_BORDER_COLOR
      ;; GTK_STYLE_PROPERTY_BACKGROUND_IMAGE
      ;; GTK_STYLE_CLASS_CELL
      ;; GTK_STYLE_CLASS_DIM_LABEL
      ;; GTK_STYLE_CLASS_ENTRY
      ;; GTK_STYLE_CLASS_LABEL
      ;; GTK_STYLE_CLASS_COMBOBOX_ENTRY
      ;; GTK_STYLE_CLASS_BUTTON
      ;; GTK_STYLE_CLASS_LIST
      ;; GTK_STYLE_CLASS_LIST_ROW
      ;; GTK_STYLE_CLASS_CALENDAR
      ;; GTK_STYLE_CLASS_SLIDER
      ;; GTK_STYLE_CLASS_BACKGROUND
      ;; GTK_STYLE_CLASS_RUBBERBAND
      ;; GTK_STYLE_CLASS_CSD
      ;; ; ...
      ;; GTK_STYLE_CLASS_PAPER
      ;; GTK_STYLE_CLASS_MONOSPACE
      ;; GTK_STYLE_CLASS_WIDE
      ;; GTK_STYLE_REGION_ROW
      ;; GTK_STYLE_REGION_COLUMN
      ;; GTK_STYLE_REGION_COLUMN_HEADER
      ;; GTK_STYLE_REGION_TAB

      gtk_style_context_new
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkStyleContext* type-vptr)
   (define gtk_style_context_get_type (GTK3 GType "gtk_style_context_get_type"))
   (define GTK_TYPE_STYLE_CONTEXT (gtk_style_context_get_type))

   (define gtk_style_context_new (GTK3 GtkStyleContext* "gtk_adjustment_new"))
   ; ...

   (define (GtkStyleContext props)
      ;...
      #false)
))
