(define-library (Gtk 3 Container)
   (description "Base class for widgets which contain other widgets")
   (export
      GtkContainer
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)
      
      (lib gtk-3 container))

(begin
   (GTK_CLASS Container Widget {
         ; Adds widget to container
         'add (lambda (widget)
            (cond
               ((vptr? widget)
                  (gtk_container_add ptr widget))
               ((GObject? widget)
                  (define child (widget 'Widget))
                  (when child
                     (gtk_container_add ptr child)))
            ))
      }
      () ; no rai

   (GTK_CLASS:CONSTRUCTORS
   ))
))
