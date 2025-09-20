(define-library (Gtk 3 Gtk)
   (description "")
   (export
      vptr? ctor?
      type-callable type-value+
      error

      GtkEventHandler G_SIGNAL_CONNECT
      ;; G_DECLARE_CLASS

      (exports (owl ff))
      (exports (lib gtk-3 gtk))
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk))

(begin
   (define (vptr? o) (eq? (type o) type-vptr))
   (define (ctor? o) (eq? (type o) type-procedure))

   (define (callable? o) (eq? (type o) type-callable))

   (define (error name . args)
      (runtime-error (string-append name ": invalid argument" (if (= (length args) 1) "" "s")) args))

   (define-syntax G_SIGNAL_CONNECT
      (syntax-rules (ptr handler
                     cond else print; lisp reserved
                     G_CALLBACK GTK_CALLBACK g_signal_connect) ; GLib
         ((G_SIGNAL_CONNECT name . body)
            (lambda (handler)
               (define callback (cond
                  ((and (eq? (type handler) type-value+) ; already cooked
                        (function? (vm:deref handler)))
                     handler)
                  ((and (eq? (type handler) type-value+) ; make a new one
                        (pair? (vm:deref handler))
                        (function? (cdr (vm:deref handler))))
                     (G_CALLBACK handler))
                  ((function? handler)
                     . body)
                  (else
                     (runtime-error (string-append "GObject: invalid 'on-" name " handler") (list handler)))))
               (g_signal_connect ptr name callback #false)) )))

   (define-syntax GtkEventHandler
      (syntax-rules (ctor ptr handler ; 
                     cond else print ; lisp reserved
                     G_CALLBACK GTK_CALLBACK g_signal_connect) ; GLib
         ((GtkEventHandler name ())
            (G_SIGNAL_CONNECT name
               (G_CALLBACK (GTK_CALLBACK (self userdata)
                  (handler (ctor ctor self {
                     ; 'userdata #f
                  }))))))
      ))

   ;; (define-syntax G_DECLARE_CLASS
   ;;    (syntax-rules (base ptr)
   ;;       ((G_DECLARE_CLASS name super (body))
   ;;          (ff-replace base {
   ;;             'class name  'superclass super
   ;;             'super base

   ;;             'Label ptr
   ;;             body
   ;;          }))
   ;;    ))

))
