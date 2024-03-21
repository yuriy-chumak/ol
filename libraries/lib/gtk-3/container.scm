(define-library (lib gtk-3 container)
   (description "Base class for widgets which contain other widgets")
   (export
      GtkContainer*
      GTK_TYPE_CONTAINER
      gtk_container_get_type

      gtk_container_add
      gtk_container_remove

      gtk_container_foreach

      ; lisp
      GtkContainer
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define GtkContainer* type-vptr)
   (define gtk_container_get_type (GTK3 GType "gtk_container_get_type"))
   (define GTK_TYPE_CONTAINER (gtk_container_get_type))

   (define gtk_container_add (GTK3 void "gtk_container_add" GtkContainer* GtkWidget*))
   (define gtk_container_remove (GTK3 void "gtk_container_remove" GtkContainer* GtkWidget*))

   (define gtk_container_foreach (GTK3 fft-void "gtk_container_foreach" GtkContainer* GtkCallback gpointer))

   (define GtkContainer
      (define (make ptr options)
         (define base (GtkWidget ptr options))
         (define this (ff-replace base {

            ; Adds widget to container.
            'add (lambda (widget)
               (when (GObject? widget)
                  (define child (widget 'widget))
                  (when child
                     (gtk_container_add ptr child))))

            'super base
         }))
         ; no options handling yet
         (GObject this))
   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               (else
                  (runtime-error "GtkContainer: invalid argument" a1)) ))
      ((a1 op) (cond
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkContainer: invalid arguments" (cons a1 op)))))
   ))
))
