(define-library (lib gtk-3 message-dialog)
   (export
      GtkMessageDialog
      GtkMessageDialog*

      gtk_message_dialog_new

      GtkButtonsType
      GTK_BUTTONS_NONE
      GTK_BUTTONS_OK
      GTK_BUTTONS_CLOSE
      GTK_BUTTONS_CANCEL
      GTK_BUTTONS_YES_NO
      GTK_BUTTONS_OK_CANCEL

      GtkMessageType
      GTK_MESSAGE_INFO
      GTK_MESSAGE_WARNING
      GTK_MESSAGE_QUESTION
      GTK_MESSAGE_ERROR
      GTK_MESSAGE_OTHER

      (exports (lib gtk-3 dialog))
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget)
      (lib gtk-3 window)
      (lib gtk-3 dialog))

(begin
   (define GtkMessageDialog* type-vptr)

   (define GtkButtonsType gint)
   (define GTK_BUTTONS_NONE 0)
   (define GTK_BUTTONS_OK 1)
   (define GTK_BUTTONS_CLOSE 2)
   (define GTK_BUTTONS_CANCEL 3)
   (define GTK_BUTTONS_YES_NO 4)
   (define GTK_BUTTONS_OK_CANCEL 5)

   (define GtkMessageType gint)
   (define GTK_MESSAGE_INFO 0)
   (define GTK_MESSAGE_WARNING 1)
   (define GTK_MESSAGE_QUESTION 2)
   (define GTK_MESSAGE_ERROR 3)
   (define GTK_MESSAGE_OTHER 4)

   (define gtk_message_dialog_new (GTK3 GtkWidget* "gtk_message_dialog_new" GtkWindow* GtkDialogFlags GtkMessageType GtkButtonsType gchar*))

   (define (GtkMessageDialog owner props . args)
      (define dialog (apply gtk_message_dialog_new (cons*
         owner
         (props 'flags GTK_DIALOG_DESTROY_WITH_PARENT)
         (props 'type GTK_MESSAGE_ERROR)
         (props 'buttons GTK_BUTTONS_CLOSE)
         (props 'message "My God, it's full of stars!")
         args)))

      (define result (gtk_dialog_run dialog))
      (gtk_widget_destroy dialog)
      result)
))
