(define-library (lib gtk-3 tree-store)
   (export
      GtkTreeStore*
      GTK_TYPE_TREE_STORE
      gtk_tree_store_get_type

      gtk_tree_store_append
      gtk_tree_store_set_value
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk)
      (lib gtk-3 tree-model))

(begin
   (define GtkTreeStore* type-vptr)
   (define gtk_tree_store_get_type (GTK3 GType "gtk_tree_store_get_type"))
   (define GTK_TYPE_TREE_STORE (gtk_tree_store_get_type))

   (define gtk_tree_store_append (GTK3 void "gtk_tree_store_append" GtkTreeStore* GtkTreeIter* GtkTreeIter*))
   (define gtk_tree_store_set_value (GTK3 void "gtk_tree_store_set_value" GtkTreeStore* GtkTreeIter* gint GValue*))

   (define (GtkTreeStore props)
      ;...
      #false)
))
