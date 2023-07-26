#!/usr/bin/env ol
(import
      (lib glib-2)
      (lib gtk-3))

(import (otus ffi))
(import (lib sqlite))
(define database (make-sqlite3))
(sqlite3_open "url-album.sqlite" database)
(sqlite:query database "
   CREATE TABLE IF NOT EXISTS notes (
      id INTEGER PRIMARY KEY,
      parent INTEGER REFERENCES notes(id),
      name TEXT,

      url TEXT,
      preview BLOB,
      created DATETIME DEFAULT CURRENT_TIMESTAMP
   )")
(sqlite:query database "
   INSERT OR IGNORE INTO notes (id, name) VALUES (0, '/')")


(import (lib gtk-3 entry))
(import (lib gtk-3 tree-model))
(import (lib gtk-3 tree-store))
(import (lib gtk-3 tree-view-column))
(import (lib gtk-3 tree-view))

; main:
; ------------------------------------------------------------
(gtk_init '(0) #f)
; gtk_builder_add_callback_symbol

; load and decode a file
(define builder (gtk_builder_new_from_file "url-album.glade"))

; get window from template
(define window (gtk_builder_get_object builder "window"))
(define URL (gtk_builder_get_object builder "URL"))
(define URL-EDITOR (gtk_builder_get_object builder "URL-EDITOR"))

(define EDIT (gtk_builder_get_object builder "EDIT"))

; ui elements
(define ui-treeview (gtk_builder_get_object builder "TREE"))

; data elements:
(define store (gtk_builder_get_object builder "STORE"))

; builder is no more required, let's free a system resource
; ---------------------------------------------------------
; todo: add (urlencode) function
(define (set-url! url)
   (define encoded (s/&/&amp;/ url))
   (gtk_label_set_markup URL (string-append "<a href=\"" encoded "\">" encoded "</a>"))
   (gtk_widget_show URL)
   (gtk_widget_hide URL-EDITOR))



; добавим и выделим корневой элемент:
(let ((root (make-GtkTreeIter))
      (path (gtk_tree_path_new_from_string "0")))
   (gtk_tree_store_append store root NULL)
   (gtk_tree_store_set_value store root 0 (make-GValue 0))
   (gtk_tree_store_set_value store root 1 (make-GValue
      (sqlite:value database "SELECT name FROM notes WHERE id=0")))
   (gtk_tree_store_set_value store root 2 (make-GValue 0))
   (gtk_tree_view_set_cursor ui-treeview path NULL #false)

   ; восстановим все узлы из БД
   ; todo: сделать одним запросом к БД, с "ORDER BY parent_id,id"
   (let update ((parent root) (parent_id 0))
      (for-each (lambda (row)
            (define child (make-GtkTreeIter))
            (define id (ref row 1))
            (define name (ref row 2))
            (gtk_tree_store_append store child parent)
            (gtk_tree_store_set_value store child 0 (make-GValue id))
            (gtk_tree_store_set_value store child 1 (make-GValue name))
            (gtk_tree_store_set_value store child 2 (make-GValue 1))
            (update child id))
         (sqlite:map (sqlite:query database "
               SELECT id,name
                  FROM notes
                  WHERE parent = ?
                  ORDER BY id" parent_id)
            (lambda (id name) [id name]))))

   (gtk_tree_view_expand_row ui-treeview path 1)
   (gtk_tree_path_free path))


; close button processor
(define quit
   (GTK_CALLBACK (widget userdata)
      (print "Close pressed. Bye-bye.")
      ; todo: save window size, etc.
      (gtk_main_quit)))
(g_signal_connect window "destroy" (G_CALLBACK quit) NULL)

; add new url processor
(define ADD
   (GTK_CALLBACK (widget userdata)
      (define model (gtk_tree_view_get_model ui-treeview))

      (define path (make-vptr))
      (gtk_tree_view_get_cursor ui-treeview path NULL)
      (define iter (make-GtkTreeIter))
      (gtk_tree_model_get_iter model iter path)

      (define parent_id (let ((g (make-GValue)))
         (gtk_tree_model_get_value model iter 0 g)
         (g_value_get_int g)))

      (define name "new bookmark")

      (define id (sqlite:value database
         "INSERT INTO notes (parent, name) VALUES(?,?) RETURNING id"
         parent_id name))
      (when id
         (define child (make-GtkTreeIter))
         (gtk_tree_store_append store child iter)
         (gtk_tree_store_set_value store child 0 (make-GValue id))
         (gtk_tree_store_set_value store child 1 (make-GValue name))
         (gtk_tree_store_set_value store child 2 (make-GValue 1))

         ;(gtk_tree_view_expand_row ui-treeview path 0)
         (define path (gtk_tree_model_get_path model child))
         (gtk_tree_view_set_cursor ui-treeview path NULL #f)
         (gtk_tree_path_free path))
      ; and free resources
      (gtk_tree_path_free path)
      TRUE))
(gtk_builder_add_callback_symbol builder "ADD" (G_CALLBACK ADD))


;; (define row-activated
;;    (GTK_CALLBACK (self path column user_data)
;;       (define model (gtk_tree_view_get_model ui-treeview))
;;       (define iter (make-GtkTreeIter))
;;       (gtk_tree_model_get_iter model iter path)
;;       (define id (let ((g (make-GValue)))
;;          (gtk_tree_model_get_value model iter 0 g)
;;          (g_value_get_int g)))
;;       (define url (sqlite:value database "SELECT url FROM notes WHERE id=?+0" id))
;;       (gtk_entry_set_text URL (if url url "-- // --"))
;;       TRUE))
;; (g_signal_connect ui-treeview "row-activated" (G_CALLBACK row-activated) NULL)

(define cursor-changed
   (GTK_CALLBACK (self user_data)
      (define model (gtk_tree_view_get_model ui-treeview))
      (define path (make-vptr))
      (gtk_tree_view_get_cursor ui-treeview path NULL)
      (define iter (make-GtkTreeIter))
      (gtk_tree_model_get_iter model iter path)
      (define id (let ((g (make-GValue)))
         (gtk_tree_model_get_value model iter 0 g)
         (g_value_get_int g)))
      (define url (sqlite:value database "SELECT url FROM notes WHERE id=?+0" id))
      (set-url! (if url url "-- // --"))

      TRUE))
(g_signal_connect ui-treeview "cursor-changed" (G_CALLBACK cursor-changed) NULL)

(define name-changed
   (vm:pin (cons
      (cons void (list GObject* type-string type-string gpointer))
      (lambda (self path text userdata)
         (define model (gtk_tree_view_get_model ui-treeview))
         (define path (make-vptr))
         (gtk_tree_view_get_cursor ui-treeview path NULL)
         (define iter (make-GtkTreeIter))
         (gtk_tree_model_get_iter model iter path)
         (define id (let ((g (make-GValue)))
            (gtk_tree_model_get_value model iter 0 g)
            (g_value_get_int g)))

         (define name (sqlite:value database "UPDATE OR FAIL notes SET name=?2 WHERE id=?1 RETURNING name" id text))
         (if name
            (gtk_tree_store_set_value store iter 1 (make-GValue name)))

         TRUE))))
(gtk_builder_add_callback_symbol builder "name-changed" (G_CALLBACK name-changed))

(set-url! "http://google.com")

;; (define activate-link
;;    (vm:pin (cons
;;       (cons gint (list GObject* type-string gpointer))
;;       (lambda (self url userdata)
;;          (print "url activated")

;;          TRUE))))
;; (g_signal_connect URL "activate-link" (G_CALLBACK activate-link) NULL)




; add new url processor
(define edit-clicked
   (GTK_CALLBACK (self userdata)
      (define model (gtk_tree_view_get_model ui-treeview))
      (define path (make-vptr))
      (gtk_tree_view_get_cursor ui-treeview path NULL)
      (define iter (make-GtkTreeIter))
      (gtk_tree_model_get_iter model iter path)
      (define id (let ((g (make-GValue)))
         (gtk_tree_model_get_value model iter 0 g)
         (g_value_get_int g)))

      (define url (sqlite:value database "SELECT url FROM notes WHERE id=?" id))
      
      (gtk_widget_hide URL)
      (gtk_entry_set_text URL-EDITOR (if url url "enter your url here"))
      (gtk_widget_show URL-EDITOR)
      (gtk_widget_grab_focus URL-EDITOR)

      TRUE))
(g_signal_connect EDIT "clicked" (G_CALLBACK edit-clicked) NULL)

(define activate
   (GTK_CALLBACK (self user_data)
      (define model (gtk_tree_view_get_model ui-treeview))
      (define path (make-vptr))
      (gtk_tree_view_get_cursor ui-treeview path NULL)
      (define iter (make-GtkTreeIter))
      (gtk_tree_model_get_iter model iter path)
      (define id (let ((g (make-GValue)))
         (gtk_tree_model_get_value model iter 0 g)
         (g_value_get_int g)))

      (define text (gtk_entry_get_text URL-EDITOR))
      (define url (sqlite:value database "UPDATE OR FAIL notes SET url=?2 WHERE id=?1 RETURNING url" id text))
      (if url
         (set-url! url))
      (gtk_widget_show URL)
      (gtk_widget_hide URL-EDITOR)

      TRUE))
(g_signal_connect URL-EDITOR "activate" (G_CALLBACK activate) NULL)

; handle url rename

; todo: speedup
;;   model = gtk_tree_view_get_model(GTK_TREE_VIEW(view));
;;   g_object_ref(model); /* Make sure the model stays with us after the tree view unrefs it */
;;   gtk_tree_view_set_model(GTK_TREE_VIEW(view), NULL); /* Detach model from view */
;;   ... insert a couple of thousand rows ...
;;   gtk_tree_view_set_model(GTK_TREE_VIEW(view), model); /* Re-attach model to view */
;;   g_object_unref(model);


; finally, connect signals
(gtk_builder_connect_signals builder #f)

; show window and run
(g_object_unref builder)
(gtk_widget_show_all window)
(gtk_widget_hide URL-EDITOR)
(gtk_main)
