(define-library (lib gtk-3 file-chooser-dialog)
   (export
      GtkFileChooserDialog*

      gtk_file_chooser_dialog_new
      gtk_file_chooser_dialog_new_with_backend

      (exports (lib gtk-3 window))
      (exports (lib gtk-3 file-chooser))
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 window)
      (lib gtk-3 widget)
      (lib gtk-3 file-chooser)
      (lib gtk-3 gtk))

(begin
   (define GtkFileChooserDialog* type-vptr)

   (define gtk_file_chooser_dialog_new4 (GTK3 GtkWidget* "gtk_file_chooser_dialog_new" type-string GtkWindow* GtkFileChooserAction fft-void*))
   (define gtk_file_chooser_dialog_new6 (GTK3 GtkWidget* "gtk_file_chooser_dialog_new" type-string GtkWindow* GtkFileChooserAction type-string fft-int fft-void*))
   (define gtk_file_chooser_dialog_new8 (GTK3 GtkWidget* "gtk_file_chooser_dialog_new" type-string GtkWindow* GtkFileChooserAction type-string fft-int type-string fft-int fft-void*))
   (define gtk_file_chooser_dialog_new (case-lambda
      ((title parent action done)
         (gtk_file_chooser_dialog_new4 title parent action #f))
      ((title parent action text1 action1 done)
         (gtk_file_chooser_dialog_new6 title parent action text1 action1 #f))
      ((title parent action text1 action1 text2 action2 done)
         (gtk_file_chooser_dialog_new8 title parent action text1 action1 text2 action2 #f))
   ))

   (define gtk_file_chooser_dialog_new_with_backend (GTK3 GtkWidget* "gtk_file_chooser_dialog_new_with_backend" type-string GtkWindow* GtkFileChooserAction type-string type-string))

))