(define-library (lib gtk-3 about-dialog)
   (export
      GtkAboutDialog*

      gtk_dialog_run
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkAboutDialog* type-vptr)

   (define gtk_dialog_run (GTK3 gint "gtk_dialog_run" GtkAboutDialog*))

))