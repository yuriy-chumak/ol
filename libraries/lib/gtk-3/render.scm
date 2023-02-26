(define-library (lib gtk-3 render)
   (description "Rendering UI elements")
   (export
      ; gtk_render_check
      ; gtk_render_option
      ; gtk_render_arrow
      gtk_render_background
      ; gtk_render_background_get_clip
      ; gtk_render_frame
      ; gtk_render_expander
      ; gtk_render_focus
      ; gtk_render_layout
      ; gtk_render_line
      ; gtk_render_slider
      ; gtk_render_frame_gap
      ; gtk_render_extension
      ; gtk_render_handle
      ; gtk_render_activity
      ; gtk_render_icon_pixbuf
      ; gtk_render_icon
      ; gtk_render_icon_surface
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gdk-3)
      (lib cairo)
      (lib gtk-3 gtk))

(begin
   ; gtk_render_check
   ; gtk_render_option
   ; gtk_render_arrow
   (define gtk_render_background (GTK3 fft-void "gtk_render_background" GtkStyleContext* cairo_t* gdouble gdouble gdouble gdouble))
   ; gtk_render_background_get_clip
   ; gtk_render_frame
   ; gtk_render_expander
   ; gtk_render_focus
   ; gtk_render_layout
   ; gtk_render_line
   ; gtk_render_slider
   ; gtk_render_frame_gap
   ; gtk_render_extension
   ; gtk_render_handle
   ; gtk_render_activity
   ; gtk_render_icon_pixbuf
   ; gtk_render_icon
   ; gtk_render_icon_surface
))
