(define-library (lib gtk-3 builder)
   (export
      GtkBuilder*

      gtk_builder_new
      gtk_builder_new_from_file
      gtk_builder_add_from_file
      gtk_builder_get_object
      gtk_builder_new_from_string
      gtk_builder_add_from_string

      gtk_builder_add_callback_symbol
      gtk_builder_connect_signals

      gtk_builder_set_application
      gtk_builder_get_application

      gtk_builder_set_translation_domain
      
      ; lisp
      GtkBuilder
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 widget)
      (lib gtk-3 application))

(begin
   (define GtkBuilder* type-vptr)
   (define gtk_builder_new (GTK3 GtkBuilder* "gtk_builder_new"))
   (define gtk_builder_new_from_file (GTK3 GtkBuilder* "gtk_builder_new_from_file" type-string))
   (define gtk_builder_add_from_file (GTK3 guint "gtk_builder_add_from_file" GtkBuilder* gchar* (fft& GError*)))
   (define gtk_builder_new_from_string (GTK3 GtkBuilder* "gtk_builder_new_from_string" type-string gssize))
   (define gtk_builder_add_from_string (GTK3 GtkBuilder* "gtk_builder_add_from_string" GtkBuilder* type-string gssize (fft& GError*)))

   ; Note that this function does not increment the reference count of the returned object:
   (define gtk_builder_get_object (GTK3 GObject* "gtk_builder_get_object" GtkBuilder* type-string))

   (define GtkBuilderConnectFunc GtkCallback) ; void (*GtkBuilderConnectFunc)(GtkBuilder *builder, GObject *object, const gchar *signal_name, const gchar *handler_name, GObject *connect_object, GConnectFlags flags, gpointer user_data)
   (define gtk_builder_add_callback_symbol (GTK3 fft-void "gtk_builder_add_callback_symbol" GtkBuilder* gchar* GCallback))
   (define gtk_builder_connect_signals (GTK3 fft-void "gtk_builder_connect_signals" GtkBuilder* gpointer))

   (define gtk_builder_set_application (GTK3 void "gtk_builder_set_application" GtkBuilder* GtkApplication*))
   (define gtk_builder_get_application (GTK3 GtkApplication* "gtk_builder_get_application" GtkBuilder*))

   (define gtk_builder_set_translation_domain (GTK3 void "gtk_builder_set_translation_domain" GtkBuilder* type-string))

   (define GtkBuilder
      (define (make ptr options)
         (define this {
            'ptr ptr ; raw pointer

            'add-from-file (lambda (filename)
               (if (> (gtk_builder_add_from_file ptr filename #f) 0)
                  #t #f))

            'add-from-string (lambda (string)
               (if (> (gtk_builder_add_from_string ptr string #f) 0)
                  #t #f))

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
                        (runtime-error "GtkWindow" "invalid handler"))))
               (gtk_builder_add_callback_symbol ptr name callback))

            ; This method is a simpler variation of gtk_builder_connect_signals_full
            'connect-signals (case-lambda
               (() (gtk_builder_connect_signals ptr #f))
               ((userdata) (gtk_builder_connect_signals ptr userdata)))

            'get-object (lambda (name)
               (gtk_builder_get_object ptr name))

            ; internals
            'super #false
         })
         ; setup and return
         (if (options 'file #f)
            ((this 'add-from-file) (options 'file)))
         (if (options 'xml #f)
            ((this 'add-from-string) (options 'file)))
         (GObject this))
   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               ((ff? a1)
                  (make (gtk_builder_new) a1))
               ((string? a1)
                  (make (gtk_builder_new_from_file a1) #f))
               (else
                  (runtime-error "GtkBuilder: invalid argument" a1)) ))
      ((a1 op) (cond
               ((and (string? a1) (ff? op))
                  (make (gtk_builder_new_from_file a1) op))
               (else
                  (runtime-error "GtkBuilder: invalid arguments" (cons a1 op))) ))
   ))

   ;;       (let ((this (cond
   ;;                      ((eq? ptr #false)
   ;;                         (gtk_builder_new))
   ;;                      ((string? props)
   ;;                         (gtk_builder_new_from_file props))
   ;;                      (else
   ;;                         (runtime-error "invalid GtkBuilder constructor" ptr)))))
   ;;          {
   ;;             'unref (lambda () (g_object_unref this))
   ;;             'get-object (lambda (o) (gtk_builder_get_object this o))
   ;;             'connect-signals (lambda (userdata) (gtk_builder_connect_signals this userdata))
   ;;          }))

   ;;    (case-lambda
   ;;       ((arg)
   ;;          (GtkBuilder arg {}))
   ;;       ((arg1 arg2)
   ;;          (GtkBuilder arg1 arg2))
   ;;       (()
   ;;          (GtkBuilder #false {})) ))
))
