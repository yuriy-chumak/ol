(define-library (lib gtk-3 text-buffer)
   (export
      GtkTextBuffer*
      gtk_text_buffer_insert_at_cursor
      gtk_text_buffer_set_text
      gtk_text_buffer_get_text

      gtk_text_buffer_get_start_iter
      gtk_text_buffer_get_end_iter
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

; https://developer.gnome.org/gtk3/stable/GtkTextBuffer.html
(begin
   (define GtkTextBuffer* fft-void*)

   (define gtk_text_buffer_insert_at_cursor (GTK3 fft-void "gtk_text_buffer_insert_at_cursor" GtkTextBuffer* type-string gint))
   (define gtk_text_buffer_set_text (GTK3 fft-void "gtk_text_buffer_set_text" GtkTextBuffer* type-string gint))
   (define gtk_text_buffer_get_text (GTK3 type-string "gtk_text_buffer_get_text" GtkTextBuffer* GtkTextIter* GtkTextIter* gboolean))

   (define gtk_text_buffer_get_start_iter (GTK3 fft-void "gtk_text_buffer_get_start_iter" GtkTextBuffer* GtkTextIter*))
   (define gtk_text_buffer_get_end_iter (GTK3 fft-void "gtk_text_buffer_get_end_iter" GtkTextBuffer* GtkTextIter*))

   (define (GtkTextBuffer props)
      ;...
      #false)
))
