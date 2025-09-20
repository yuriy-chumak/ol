(define-library (Gtk 3 Builder)
   (export
      GtkBuilder
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Widget)
      (Gtk 3 Label)
      (Gtk 3 Button)

      (Gtk 3 Window)

      (lib gtk-3 builder))

(begin
   (define GtkBuilder
      (define (make ctor ptr options)
         ; convert builder id into object
         (define (get-Object Class)
            (case-lambda
               ((id) (Class (gtk_builder_get_object ptr id)))
               ((id op)
                     (Class (gtk_builder_get_object ptr id) op))))

         (define this {
            'Ptr* ptr ; raw pointer
            'class 'Builder  'superclass #false

            'Builder ptr

            'add-from-file (lambda (filename)
               (> (gtk_builder_add_from_file ptr filename #f) 0))

            'add-from-string (lambda (string)
               (> (gtk_builder_add_from_string ptr string -1 #f) 0))

            'add-callback-symbol (lambda (name handler)
               (define callback
                  (cond
                     ((eq? (type handler) type-callable) ; callback
                        handler)
                     ((and (eq? (type handler) type-value+) ; pin?
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

            'get-Widget (get-Object GtkWidget)
            'get-Window (get-Object GtkWindow)

            'get-Label (get-Object GtkLabel)
            'get-Button (get-Object GtkButton)
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
