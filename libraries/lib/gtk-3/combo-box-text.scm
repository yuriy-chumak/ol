(define-library (lib gtk-3 combo-box-text)
   (export
      GtkComboBoxText*
      GTK_TYPE_COMBO_BOX_TEXT
      gtk_combo_box_text_get_type

      gtk_combo_box_text_append_text
      gtk_combo_box_text_insert_text
      gtk_combo_box_text_prepend_text

      gtk_combo_box_text_remove
      gtk_combo_box_text_remove_all

      gtk_combo_box_text_get_active_text
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkComboBoxText* type-vptr)
   (define gtk_combo_box_text_get_type (GTK3 GType "gtk_combo_box_text_get_type"))
   (define GTK_TYPE_COMBO_BOX_TEXT (gtk_combo_box_text_get_type))

   (define gtk_combo_box_text_append_text (GTK3 void "gtk_combo_box_text_append_text" GtkComboBoxText* type-string))
   (define gtk_combo_box_text_insert_text (GTK3 void "gtk_combo_box_text_insert_text" GtkComboBoxText* gint type-string))
   (define gtk_combo_box_text_prepend_text (GTK3 void "gtk_combo_box_text_prepend_text" GtkComboBoxText* type-string))

   (define gtk_combo_box_text_remove (GTK3 void "gtk_combo_box_text_remove" GtkComboBoxText* gint))
   (define gtk_combo_box_text_remove_all (GTK3 void "gtk_combo_box_text_remove_all" GtkComboBoxText*))

   (define :gtk_combo_box_text_get_active_text (GTK3 gpointer "gtk_combo_box_text_get_active_text" GtkComboBoxText*))
   (define (gtk_combo_box_text_get_active_text combo_box)
      (define active-text-ptr (:gtk_combo_box_text_get_active_text combo_box))
      (define active-text (vptr->string active-text-ptr))
      (g_free active-text-ptr)
      active-text)

   ; gtk_combo_box_text_insert
   ; gtk_combo_box_text_append
   ; gtk_combo_box_text_prepend

   (define (GtkComboBoxText props)
      ;...
      #false)
))
