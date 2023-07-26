(define-library (lib gtk-3 list-store)
   (export
      GtkListStore*
      GTK_TYPE_LIST_STORE
      gtk_list_store_get_type

      gtk_list_store_set_value

      gtk_list_store_clear
      ;;gtk_list_store_set_value
      ;;gtk_list_store_remove
      ;;gtk_list_store_insert
      gtk_list_store_append

      make-GtkTreeIter ; from tree-model
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 tree-model)) ; GtkTreeIter

(begin
   (define GtkListStore* type-vptr)
   (define gtk_list_store_get_type (GTK3 GType "gtk_list_store_get_type"))
   (define GTK_TYPE_LIST_STORE (gtk_list_store_get_type))

   (define gtk_list_store_set_value (GTK3 void "gtk_list_store_set_value" GtkListStore* GtkTreeIter* gint GValue*))

   (define gtk_list_store_clear (GTK3 void "gtk_list_store_clear" GtkListStore*))
   (define gtk_list_store_append (GTK3 void "gtk_list_store_append" GtkListStore* GtkTreeIter*))


))