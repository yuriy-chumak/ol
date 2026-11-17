(define-library (Gtk 3 ListStore)
   (description "Gtk List Store")
   (export
      GtkListStore
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)

      (lib gtk-3 list-store))

(begin
   (import (owl io))
   (GTK_CLASS ListStore #false
      ; methods
      {
         ; 
         'clear (lambda ()
            (gtk_list_store_clear ptr))

         ; Returns the topmost widget in the container hierarchy widget is a part of.
         'append (lambda args
            (define iter (make-GtkTreeIter))
            (for-each (lambda (id arg)
                  (gtk_list_store_append ptr iter)
                  (define value (make-GValue arg))
                  (gtk_list_store_set_value ptr iter 0 value)
                  (g_value_unset value))
               (iota (length args))
               args))
      }

      ()
      ;; ; apply options
      ;; (
      ;;    ('on-destroy . 'set-destroy-handler)
      ;;    ('on-button-press . 'set-button-press-handler)) ;?

      ; main
      (GTK_CLASS:CONSTRUCTORS))
))
