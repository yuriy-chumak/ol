(define-library (Gtk 3 Gtk)
   (description "")
   (export
      vptr? ctor?
      type-callable type-value+
      error

      GtkEventHandler
      GtkSignalHandler
      G_SIGNAL_CONNECTOR
      ;; G_DECLARE_CLASS

      (exports (owl ff))
      (exports (lib gtk-3 gtk))

      ; internal declarators
      GTK_CLASS
      GTK_CLASS:CONSTRUCTORS
      ;; GTK_CTOR_DEFAULT
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

   ; todo: possibly change to macro
   (define-syntax G_SIGNAL_CONNECTOR
      (syntax-rules (ptr handler
                     cond else print; lisp reserved
                     G_CALLBACK GTK_CALLBACK g_signal_connect) ; GLib
         ((G_SIGNAL_CONNECTOR name . body)
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
            (G_SIGNAL_CONNECTOR name
               (G_CALLBACK (GTK_CALLBACK (self userdata)
                  (handler (ctor ctor self {
                     'userdata userdata
                  }))))))
         ((GtkEventHandler name (arg1))
            (G_SIGNAL_CONNECTOR name
               (G_CALLBACK (GTK_CALLBACK (self arg1 userdata)
                  (handler (ctor ctor self {
                     'userdata userdata
                  }) arg1)))))

         ;; ((GtkEventHandler name (arg1) .body)
         ;;    (G_SIGNAL_CONNECT name
         ;;       (G_CALLBACK (GTK_CALLBACK (self userdata)
         ;;          (handler (ctor ctor self {
         ;;             ; 'userdata #f
         ;;          }))))))
      ))

   (define-syntax GtkSignalHandler
      (syntax-rules (ctor ptr handler ; 
                     cond else print ; lisp reserved
                     G_CALLBACK GTK_CALLBACK) ; GLib
         ((GtkSignalHandler (this) . body)
            (G_CALLBACK (GTK_CALLBACK (self userdata)
               ; TODO: switch for "self" internal types and use appropriate constructor
               ((lambda (this) . body) self)
            )))

      ))

   (define-macro GTK_CLASS:INIT (lambda body
      `(begin
         ,@(map (lambda (o)
               `(when (options ,(car o) #f)
                  ((this ,(cdr o)) (options ,(car o)))))
            body)
   )))

   (define-macro GTK_CLASS:CONSTRUCTORS (lambda body
      `(case-lambda
         ,@body
         ; default constructors
         ((a1) (if (vptr? a1)
                  (make make a1 #e)
                  (error gtkname a1)))
         ((a1 op) (if (and (vptr? a1) (ff? op))
                     (make make a1 op)
                     (error gtkname a1 op)))
         ; inheritance
         ((a1 a2 a3) (if (ctor? a1)
                        (make a1 a2 a3)
                        (error gtkname a1 a2 a3)))
      )
   ))

   (define-macro GTK_CLASS (lambda (name owner methods autorun . body)
      (define gtkname (string-append "Gtk" (symbol->string name)))

      `(define ,(string->symbol (string-append "Gtk" (symbol->string name)))
         (define gtkname ,gtkname)
         (define (make ctor ptr options)
            ,@(if owner (list
               `(define base (,(string->symbol (string-append "Gtk" (symbol->string owner))) ctor ptr options))
               `(define this (ff-replace
                  (ff-replace base ,(del methods 'init)) {
                     'class (quote ,name)
                     'superclass (quote ,owner)
                     'super base
                     (quote ,name) ptr
                  })))
               else (list
               `(define this (ff-replace ,(del methods 'init)
                  {
                     'Ptr* ptr ; raw pointer
                     'class (quote ,name)
                     'superclass #false
                     (quote ,name) ptr
                  }))))

            ; apply 
            ,@(map (lambda (o)
                  (if (pair? (car o)) ; smart handler of '('property . 'property-setter)
                     `(when (options ,(car o) #f)
                        ((this ,(cdr o)) (options ,(car o))))
                     o)) ; just a code
               autorun)
            
            (GObject this))

         ,@body)
   ))

))
