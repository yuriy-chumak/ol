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
   (define GtkContainer
      (define (make ctor ptr options)
         (define base (GtkWidget ctor ptr options))
         (define this (ff-replace base {
            'class 'Container  'superclass 'Widget
            'super base

            'Container ptr

            ; Adds widget to container.
            'add (lambda (widget)
               (when (GObject? widget)
                  (define child (widget 'Widget))
                  (when child
                     (gtk_container_add ptr child))))
         }))

         ;; no options handling yet
         ;; smart object
         (GObject this))

   ; main
   (case-lambda
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               (else
                  (runtime-error "GtkContainer: invalid argument" a1)) ))
      ((a1 op) (cond
               ((and (vptr? a1) (ff? op))
                  (make make a1 op))
               (else
                  (runtime-error "GtkContainer: invalid arguments" (cons a1 op)))))
      ; inheritance
      ((a1 a2 a3)
               (if (ctor? a1) ; (and (vptr? vptr)) ?
                  (make a1 a2 a3)
               else
                  (runtime-error "GtkContainer: invalid arguments" (cons* a1 a2 a3))))

   ))
))
