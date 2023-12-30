(define-library (lib gtk-3 dialog)
   (export
      GtkDialog*

      GtkDialogFlags
      GTK_DIALOG_MODAL
      GTK_DIALOG_DESTROY_WITH_PARENT
      GTK_DIALOG_USE_HEADER_BAR

      gtk_dialog_run
      GTK_RESPONSE_NONE
      GTK_RESPONSE_REJECT
      GTK_RESPONSE_ACCEPT
      GTK_RESPONSE_DELETE_EVENT
      GTK_RESPONSE_OK
      GTK_RESPONSE_CANCEL
      GTK_RESPONSE_CLOSE
      GTK_RESPONSE_YES
      GTK_RESPONSE_NO
      GTK_RESPONSE_APPLY
      GTK_RESPONSE_HELP
      
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
   (define GTK_RESPONSE_NONE         -1)
   (define GTK_RESPONSE_REJECT       -2)
   (define GTK_RESPONSE_ACCEPT       -3)
   (define GTK_RESPONSE_DELETE_EVENT -4)
   (define GTK_RESPONSE_OK           -5)
   (define GTK_RESPONSE_CANCEL       -6)
   (define GTK_RESPONSE_CLOSE        -7)
   (define GTK_RESPONSE_YES          -8)
   (define GTK_RESPONSE_NO           -9)
   (define GTK_RESPONSE_APPLY        -10)
   (define GTK_RESPONSE_HELP         -11)

   (define (GtkDialog props)
      ;...
      #false)
))
