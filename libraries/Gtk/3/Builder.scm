(define-library (Gtk 3 Builder)
   (export
      GtkBuilder
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Window)
      (Gtk 3 Label)
      ;; (lib gtk-3 application)
      (lib gtk-3 builder))

(begin
   (define GtkBuilder
      (define (make ctor ptr options)
         (define this {
            'Ptr* ptr ; raw pointer
            'class 'Builder  'superclass #false

            'Builder ptr

            'add-from-file (lambda (filename)
               (> (gtk_builder_add_from_file ptr filename #f) 0))

            'add-from-string (lambda (string)
               (> (gtk_builder_add_from_string ptr string #f) 0))

            'add-callback-symbol (lambda (name handler)
               (define callback
                  (cond
                     ((eq? (type handler) type-callable) ; callback
                        handler)
                     ((and (eq? (type handler) type-enum+) ; pin?
                           (pair? (vm:deref handler))
                           (function? (cdr (vm:deref handler))))
                        (G_CALLBACK handler))
                     (else
                        (runtime-error "GtkBuilder" "invalid handler"))))
               (gtk_builder_add_callback_symbol ptr name callback))

            ; This method is a simpler variation of gtk_builder_connect_signals_full
            'connect-signals (case-lambda
               (() (gtk_builder_connect_signals ptr #f))
               ((userdata) (gtk_builder_connect_signals ptr userdata)))

            'get-object (lambda (id)
               (gtk_builder_get_object ptr id))

            'get-Window (case-lambda
               ((id) (GtkWindow (gtk_builder_get_object ptr id)))
               ((id op)
                     (GtkWindow (gtk_builder_get_object ptr id) op)) )

            'get-Label (case-lambda
               ((id) (GtkLabel (gtk_builder_get_object ptr id)))
               ((id op)
                     (GtkLabel (gtk_builder_get_object ptr id) op)) )
         })

         ;; apply options
         ; 
         (if (options 'file #f)
            ((this 'add-from-file) (options 'file)))
         ;
         (if (options 'xml #f)
            ((this 'add-from-string) (options 'file)))

         ;; smart object
         (GObject this))

   ; main
   (case-lambda
      (()   (make make (gtk_builder_new) #e))
      ((a1) (cond
               ((vptr? a1)
                  (make make a1 #e))
               ((ff? a1)
                  (make make (gtk_builder_new) a1))
               ((string? a1)
                  (make make (gtk_builder_new_from_file a1) #e))
               (else
                  (error "GtkBuilder" a1)) ))
      ((a1 op) (cond
               ((and (string? a1) (ff? op))
                  (make make (gtk_builder_new_from_file a1) op))
               (else
                  (error "GtkBuilder" a1 op)) ))
   ))

))
