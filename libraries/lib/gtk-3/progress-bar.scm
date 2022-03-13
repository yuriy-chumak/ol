(define-library (lib gtk-3 progress-bar)
   (export
      GtkProgressBar*

      gtk_progress_bar_pulse
      gtk_progress_bar_set_text
      gtk_progress_bar_set_fraction
      gtk_progress_bar_get_fraction
      gtk_progress_bar_set_pulse_step
      gtk_progress_bar_set_inverted
      gtk_progress_bar_set_ellipsize
      gtk_progress_bar_set_show_text
   )
   (import
      (scheme core)
      (otus ffi)
      (lib pango)
      (lib gtk-3 gtk))

(begin
   (define GtkProgressBar* type-vptr)

   (define gtk_progress_bar_pulse (GTK3 void "gtk_progress_bar_pulse" GtkProgressBar*))
   (define gtk_progress_bar_set_text (GTK3 void "gtk_progress_bar_set_text" GtkProgressBar* gchar*))
   (define gtk_progress_bar_set_fraction (GTK3 void "gtk_progress_bar_set_fraction" GtkProgressBar* gdouble))
   (define gtk_progress_bar_get_fraction (GTK3 gdouble "gtk_progress_bar_get_fraction" GtkProgressBar*))
   (define gtk_progress_bar_set_pulse_step (GTK3 void "gtk_progress_bar_set_pulse_step" GtkProgressBar* gdouble))
   (define gtk_progress_bar_set_inverted (GTK3 void "gtk_progress_bar_set_inverted" GtkProgressBar* gboolean))
   (define gtk_progress_bar_set_ellipsize (GTK3 void "gtk_progress_bar_set_ellipsize" GtkProgressBar* PangoEllipsizeMode))
   (define gtk_progress_bar_set_show_text (GTK3 void "gtk_progress_bar_set_show_text" GtkProgressBar* gboolean))

))
