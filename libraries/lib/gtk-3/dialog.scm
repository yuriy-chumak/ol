(define-library (lib gtk-3 dialog)
   (export
      GtkDialog*

      GtkDialogFlags
      GTK_DIALOG_MODAL
      GTK_DIALOG_DESTROY_WITH_PARENT
      GTK_DIALOG_USE_HEADER_BAR

      gtk_dialog_run
      
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkDialog* type-vptr)

   (define GtkDialogFlags gint)

   (define GTK_DIALOG_MODAL               1)
   (define GTK_DIALOG_DESTROY_WITH_PARENT 2)
   (define GTK_DIALOG_USE_HEADER_BAR      4)

   (define gtk_dialog_run (GTK3 gint "gtk_dialog_run" GtkDialog*))

   (define (GtkDialog props)
      ;...
      #false)
))
