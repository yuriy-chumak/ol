(define-library (Gtk 3 Widget)
   (description "Base class for all widgets")
   (export
      GtkWidget
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)

      (lib gtk-3 widget)
      (lib gtk-3 style-context)
      (lib gtk-3 style-provider))

(begin
   (import (owl io))
   (define GtkWidget
      (define (make ctor ptr options)
         (define this {
            'Ptr* ptr  ; raw pointer
            'class 'Widget  'superclass #false

            'Widget ptr

            ; Recursively shows a widget, and any child widgets.
            'show-all (lambda ()
               (gtk_widget_show_all ptr))

            ; Returns the topmost widget in the container hierarchy widget is a part of.
            'get-toplevel (lambda ()
               (gtk_widget_get_toplevel ptr))

            ; Signals that all holders of a reference to the widget should release the reference that they hold.
            'set-destroy-handler (GtkEventHandler "destroy" ())

            ; StyleContext manipulations
            'add-provider (lambda (css)
               (define context (gtk_widget_get_style_context ptr))
               (gtk_style_context_add_provider context (css 'CssProvider) GTK_STYLE_PROVIDER_PRIORITY_APPLICATION))
            'add-class (lambda (class)
               (define context (gtk_widget_get_style_context ptr))
               (gtk_style_context_add_class context class))
            'remove-class (lambda (class)
               (define context (gtk_widget_get_style_context ptr))
               (gtk_style_context_remove_class context class))
            'has-class? (lambda (class)
               (define context (gtk_widget_get_style_context ptr))
               (gtk_style_context_has_class context class))

            ; todo:
            'add-css #false

            'enable (lambda ()
               (gtk_widget_set_sensitive ptr #t))
            'disable (lambda ()
               (gtk_widget_set_sensitive ptr #f))

            ;; ; Emitted when a button (typically from a mouse) is pressed.
            ;; 'set-button-press-handler (lambda (handler)
            ;;    ; todo: convert to macro
            ;;    (define callback
            ;;       (cond
            ;;          ((eq? (type handler) type-callable) ; callback
            ;;             handler)
            ;;          ((and (eq? (type handler) type-enum+) ; pin?
            ;;                (pair? (vm:deref handler))
            ;;                (function? (cdr (vm:deref handler))))
            ;;             (G_CALLBACK handler))
            ;;          ((function? handler)
            ;;             (G_CALLBACK
            ;;                (vm:pin (cons
            ;;                   (cons gint (list GtkWidget* type-vptr))
            ;;                   (lambda (widget userdata)
            ;;                      (handler (ctor ctor widget #e)))))))
            ;;          (else
            ;;             (runtime-error "GtkWidget: invalid handler" handler))))
            ;;    (g_signal_connect ptr "button-press-event" callback #f))

         })

         ;; apply options
         ;
         (if (options 'on-destroy #f)
            ((this 'set-destroy-handler) (options 'on-destroy)))
         ; 
         (if (options 'on-button-press #f)
            ((this 'set-button-press-handler) (options 'on-button-press)))
         
         ;; smart object
         (GObject this))

   ; main
   (case-lambda
      ((a1) (cond
               ((vptr? a1) ; (GtkWidget (gtk_builder_get_object ...))
                  (make make a1 #e))
               (else
                  (error "GtkWidget" a1)) ))
      ((a1 op) (cond
               ((integer? a1) ; GType, legacy call fallback
                  (make make (gtk_widget_new a1 op) #e))
               ((and (vptr? a1) (ff? op)) ; (GtkWidget ptr {})
                  (make make a1 op))
               (else
                  (error "GtkWidget" a1 op)) ))
      ; inheritance
      ((a1 a2 a3) (cond
               ((ctor? a1)
                  (make a1 a2 a3))
               ((integer? a1)  ; GType, legacy call fallback
                  (make make (apply gtk_widget_new (cons* a1 a2 a3)) #e))
               (else
                  (error "GtkWidget" a1 a2 a3)) ))

      ; legacy (native) call
      ((a1 . pr) (cond
               ((integer? a1) ; GType
                  (make make (apply gtk_widget_new (cons a1 pr)) #e))
               (else
                  (apply error (cons* "GtkWidget" a1 pr))) ))
   ))
))
