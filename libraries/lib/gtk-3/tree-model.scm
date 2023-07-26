(define-library (lib gtk-3 tree-model)
   (export
      GtkTreeIter*
      GtkTreePath*
      GtkTreeRowReference*
      GtkTreeModel*
      GtkTreeModelIface*

      gtk_tree_path_new
      gtk_tree_path_new_from_string
      gtk_tree_path_free

      gtk_tree_model_get_iter
      gtk_tree_model_get_path
      gtk_tree_model_get_value

      make-GtkTreeIter
   )
   (import
      (scheme core)
      (otus ffi)
      (lib gtk-3 gtk))

(begin
   (define GtkTreeIter* type-vptr)
   (define GtkTreePath* type-vptr)
   (define GtkTreeRowReference* type-vptr)
   (define GtkTreeModel* type-vptr)
   (define GtkTreeModelIface* type-vptr)

   (import (owl math))
   (define (make-GtkTreeIter)
      (make-bytevector (* (size NULL) 4))) ; (sizeof _GtkTreeIter)

   (define gtk_tree_path_new             (GTK3 GtkTreePath* "gtk_tree_path_new"))
   (define gtk_tree_path_new_from_string (GTK3 GtkTreePath* "gtk_tree_path_new_from_string" type-string)) ; a colon separated list of numbers, "0:14:2"

   (define gtk_tree_path_free (GTK3 void "gtk_tree_path_free" GtkTreePath*))

   (define gtk_tree_model_get_iter (GTK3 gboolean "gtk_tree_model_get_iter" GtkTreeModel* GtkTreeIter* GtkTreePath*))
   (define gtk_tree_model_get_path (GTK3 GtkTreePath* "gtk_tree_model_get_path" GtkTreeModel* GtkTreeIter*))
   (define gtk_tree_model_get_value (GTK3 void "gtk_tree_model_get_value" GtkTreeModel* GtkTreeIter* gint GValue*))

   ;; (define :gtk_tree_model_get_string_from_iter (GTK3 gpointer "gtk_tree_model_get_string_from_iter" GtkComboBoxText*))
   ;; (define (gtk_tree_model_get_string_from_iter combo_box)
   ;;    ;; (define active-text-ptr (:gtk_combo_box_text_get_active_text combo_box))
   ;;    ;; (define active-text (vptr->string active-text-ptr))
   ;;    (g_free active-text-ptr)
   ;;    active-text)

   (define (GtkTreePath props)
      ;...
      #false)
))
