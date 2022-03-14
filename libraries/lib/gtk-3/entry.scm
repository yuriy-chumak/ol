(define-library (lib gtk-3 entry)
   (export
      GtkEntry*

      gtk_entry_get_text
      gtk_entry_set_text
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkEntry* type-vptr)

   (define gtk_entry_get_text (GTK3 type-string "gtk_entry_get_text" GtkEntry*))
   (define gtk_entry_set_text (GTK3 void "gtk_entry_set_text" GtkEntry* type-string))

   (define (GtkEntry props)
      ;...
      #false)
))
