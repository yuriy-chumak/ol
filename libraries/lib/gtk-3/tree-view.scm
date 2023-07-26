(define-library (lib gtk-3 tree-view)
   (export
      GtkTreeView*
      GTK_TYPE_TREE_VIEW
      gtk_tree_view_get_type

      gtk_tree_view_get_model

      gtk_tree_view_get_cursor
      gtk_tree_view_set_cursor

      gtk_tree_view_expand_row
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkTreeView* type-vptr)
   (define gtk_tree_view_get_type (GTK3 GType "gtk_tree_view_get_type"))
   (define GTK_TYPE_TREE_VIEW (gtk_tree_view_get_type))

   (import
      (lib gtk-3 tree-model)
      (lib gtk-3 tree-view-column))

   (define gtk_tree_view_get_model (GTK3 GtkTreeModel* "gtk_tree_view_get_model" GtkTreeView*))

   
   (define gtk_tree_view_get_cursor (GTK3 void "gtk_tree_view_get_cursor" GtkTreeView* (fft& GtkTreePath*) (fft& GtkTreeViewColumn*)))
   (define gtk_tree_view_set_cursor (GTK3 void "gtk_tree_view_set_cursor" GtkTreeView* GtkTreePath* GtkTreeViewColumn* gboolean))

   (define gtk_tree_view_expand_row (GTK3 gboolean "gtk_tree_view_expand_row" GtkTreeView* GtkTreePath* gboolean))

   ;; (define :gtk_combo_box_text_get_active_text (GTK3 gpointer "gtk_combo_box_text_get_active_text" GtkComboBoxText*))
   ;; (define (gtk_combo_box_text_get_active_text combo_box)
   ;;    (define active-text-ptr (:gtk_combo_box_text_get_active_text combo_box))
   ;;    (define active-text (vptr->string active-text-ptr))
   ;;    (g_free active-text-ptr)
   ;;    active-text)

   (define (GtkTreeView props)
      ;...
      #false)
))
