(define-library (lib gtk-3 filechooser)
   (export
      GtkFileChooserAction
      GTK_FILE_CHOOSER_ACTION_OPEN
      GTK_FILE_CHOOSER_ACTION_SAVE
      GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER
      GTK_FILE_CHOOSER_ACTION_CREATE_FOLDE

   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkFileChooserAction fft-unsigned-int)

   (define GTK_FILE_CHOOSER_ACTION_OPEN          0)
   (define GTK_FILE_CHOOSER_ACTION_SAVE          1)
   (define GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER 2)
   (define GTK_FILE_CHOOSER_ACTION_CREATE_FOLDE  3)

))