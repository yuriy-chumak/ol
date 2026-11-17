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
   (GTK_CLASS Widget #false
      ; methods
      {
         ; Recursively shows a widget, and any child widgets.
         'show-all (lambda ()
               (gtk_widget_show_all ptr))

         ; Returns the topmost widget in the container hierarchy widget is a part of.
         'get-toplevel (lambda ()
               (gtk_widget_get_toplevel ptr))

         'get-parent (lambda ()
               ;(ctor ctor (gtk_widget_get_parent ptr) {}))
               (gtk_widget_get_parent ptr))

         ; Gets the size request that was explicitly set for the widget using gtk_widget_set_size_request
         'get-size-request (lambda ()
               (define width (box 0))
               (define height (box 0))
               (gtk_widget_get_size_request ptr width height)
               (cons width height))

         'set-size-request (lambda (width height)
               (gtk_widget_set_size_request ptr width height))

         'queue-allocate (lambda ()
               (gtk_widget_queue_allocate ptr))
         'queue-resize (lambda ()
               (gtk_widget_queue_resize ptr))
         'queue-draw (lambda ()
               (gtk_widget_queue_draw ptr))

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
         ;;          ((and (eq? (type handler) type-value+) ; pin?
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
         ;;    ; todo: enable handling with gtk_widget_add_events(widget, GDK_BUTTON_PRESS_MASK);
         ;;    (g_signal_connect ptr "button-press-event" callback #f))

         'set-size-allocate-handler (lambda (handler)
            (define callback
               (cond
                  ((eq? (type handler) type-callable) ; callback
                     handler)
                  ((and (eq? (type handler) type-value+) ; pin?
                        (pair? (vm:deref handler))
                        (function? (cdr (vm:deref handler))))
                     (G_CALLBACK handler))
                  ((function? handler)
                     (G_CALLBACK
                        (vm:pin (cons
                           (cons gint (list GtkWidget* GtkAllocation* gpointer))
                           (lambda (widget allocation userdata)
                              (handler (ctor ctor widget {
                                    'userdata userdata
                                 }) allocation))))))
                  (else
                     (runtime-error "GtkWidget: invalid handler" handler))))
            (g_signal_connect ptr "size-allocate" callback #f))
      }

      ; apply options
      (
         ('on-destroy . 'set-destroy-handler)
         ('on-size-allocate . 'set-size-allocate-handler)
         ('on-button-press . 'set-button-press-handler) ) ;?

      ; main
      (GTK_CLASS:CONSTRUCTORS
         ((a1) (if (vptr? a1) ; temp, check and remove
                  (make make a1 #e)
                  (error gtkname a1)))
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
