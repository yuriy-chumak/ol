(define-library (lib gtk-3 entry)
   (description "A single line text entry field")
   (export
      GtkEntry*
      GTK_TYPE_ENTRY
      gtk_entry_get_type

      gtk_entry_get_text
      gtk_entry_set_text

      gtk_editable_set_editable ; temp
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkEntry* type-vptr)
   (define gtk_entry_get_type (GTK3 GType "gtk_entry_get_type"))
   (define GTK_TYPE_ENTRY (gtk_entry_get_type))

   (define gtk_entry_get_text (GTK3 type-string "gtk_entry_get_text" GtkEntry*))
   (define gtk_entry_set_text (GTK3 void "gtk_entry_set_text" GtkEntry* type-string))

   (define gtk_editable_set_editable (GTK3 void "gtk_editable_set_editable" GtkEntry* gboolean))

   (define (GtkEntry props)
      ;...
      #false)
))
