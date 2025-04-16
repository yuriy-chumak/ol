(define-library (gtk-3 widget)
   (description "Base class for all widgets")
   (export
      GtkWidget
      GtkEventHandler
   )
   (import
      (scheme core)
      (otus ffi) (owl ff)
      (lib gdk-3)
      (lib gtk-3 gtk)
      (lib gtk-3 widget))

(begin
   (define-syntax GtkEventHandler
      (syntax-rules (make if then else handler g_signal_connect ptr GTK_CALLBACK)
         ((GtkEventHandler name declaration . implementation)
            (lambda (handler)
               (define callback
                  (if (and (eq? (type handler) type-enum+) ; pin?
                           (function? (vm:deref handler)))
                     handler
                  else (if (and (eq? (type handler) type-enum+) ; pin?
                                (pair? (vm:deref handler))
                                (function? (cdr (vm:deref handler))))
                     (G_CALLBACK handler)
                  else (if (function? handler)
                     (GTK_CALLBACK declaration
                        (handler . implementation))
                  else
                     (runtime-error "GObject: invalid handler" handler)))))
               (g_signal_connect ptr name (G_CALLBACK callback) #false)))))

   (define GtkWidget
      (define (make ptr options)
         (define this {
            ; internals
            'ptr ptr ; raw pointer
            'widget ptr ; same pointer

            ; Recursively shows a widget, and any child widgets.
            'show-all (lambda ()
               (gtk_widget_show_all ptr))

            ; Returns the topmost widget in the container hierarchy widget is a part of.
            'get-toplevel (lambda ()
               (gtk_widget_get_toplevel ptr))

            ; Signals that all holders of a reference to the widget should release the reference that they hold.
            'set-destroy-handler (GtkEventHandler "destroy" (widget userdata)
                     (make widget #e))

            ; Emitted when a button (typically from a mouse) is pressed.
            'set-button-press-handler (lambda (handler)
               ; todo: convert to macro
               (define callback
                  (cond
                     ((eq? (type handler) type-callable) ; callback
                        handler)
                     ((and (eq? (type handler) type-enum+) ; pin?
                           (pair? (vm:deref handler))
                           (function? (cdr (vm:deref handler))))
                        (G_CALLBACK handler))
                     ((function? handler)
                        (G_CALLBACK
                           (vm:pin (cons
                              (cons gint (list GtkWidget* type-vptr))
                              (lambda (widget userdata)
                                 (handler (make widget #e)))))))
                     (else
                        (runtime-error "GtkWidget: invalid handler" handler))))
               (g_signal_connect ptr "button-press-event" callback #f))

            ; internal
            'super #false
         })
         (if (options 'on-destroy #f)
            ((this 'set-destroy-handler) (options 'on-destroy)))
         (if (options 'on-button-press #f)
            ((this 'set-button-press-handler) (options 'on-button-press)))
         (GObject this))
   ; main
   (case-lambda
      ((a1) (cond
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               (else
                  (runtime-error "GtkWidget: invalid argument" a1)) ))
      ((a1 op) (cond
               ((integer? a1) ; GType
                  (make (gtk_widget_new a1 op) #e))
               ((and (eq? (type a1) type-vptr) (ff? op))
                  (make a1 op))
               (else
                  (runtime-error "GtkWidget: invalid arguments" (cons a1 op))) ))
      ((a1 . pr) (cond
               ((integer? a1) ; GType
                  (make (apply gtk_widget_new (cons a1 pr)) #e))
               (else
                  (runtime-error "GtkWidget: invalid arguments" (cons a1 pr))) ))
   ))
))
