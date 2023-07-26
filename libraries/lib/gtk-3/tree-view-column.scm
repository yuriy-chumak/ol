(define-library (lib gtk-3 tree-view-column)
   (export
      GtkTreeViewColumn*
      GTK_TYPE_TREE_VIEW_COLUMN
      gtk_tree_view_column_get_type

      ;gtk_combo_box_text_get_active_text
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkTreeViewColumn* type-vptr)
   (define gtk_tree_view_column_get_type (GTK3 GType "gtk_tree_view_column_get_type"))
   (define GTK_TYPE_TREE_VIEW_COLUMN (gtk_tree_view_column_get_type))

   ;(define gtk_tree_view_set_cursor (GTK3 void "gtk_tree_view_set_cursor" GtkTreeView* GtkTreePath* GtkTreeViewColumn* gboolean))

   (define (GtkTreeViewColumn props)
      ;...
      #false)
))
