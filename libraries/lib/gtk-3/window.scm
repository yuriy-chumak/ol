(define-library (lib gtk-3 window)
   (export
      GtkWindow*

      gtk_window_new
      gtk_window_set_title
      gtk_window_set_default_size
      gtk_window_set_application

      gtk_window_get_size
      gtk_window_get_default_size
      gtk_window_resize
      gtk_window_present

      ; lisp interface
      GtkWindow
   )
   (import
      (scheme base)
      (otus ffi) (owl ff)
      (lib gtk-3 gtk)
      (lib gtk-3 container)
      (lib gtk-3 application)
      (lib gtk-3 widget))

(begin
   (define GtkWindow* type-vptr)
   (define GtkWindowType fft-int)

   (define gtk_window_new (GTK3 GtkWidget* "gtk_window_new" GtkWindowType))
   (define gtk_window_set_title (GTK3 void "gtk_window_set_title" GtkWindow* type-string))
   (define gtk_window_set_default_size (GTK3 void "gtk_window_set_default_size" GtkWindow* gint gint))
   (define gtk_window_set_application (GTK3 void "gtk_window_set_application" GtkWindow* GtkApplication*))

   (define gtk_window_get_size (GTK3 void "gtk_window_get_size" GtkWindow* (fft& gint) (fft& gint)))
   (define gtk_window_get_default_size (GTK3 void "gtk_window_get_default_size" GtkWindow* (fft& gint) (fft& gint)))
   (define gtk_window_resize (GTK3 void "gtk_window_resize" GtkWindow* gint gint))
   (define gtk_window_present (GTK3 void "gtk_window_present" GtkWindow*))

   ; lisp interface
   (define GtkWindow
      (define (make ptr options)
         (define this {
            'ptr ptr

            'set-title (lambda (title)
               (gtk_window_set_title ptr title))
            'set-default-size (lambda (width height)
               (gtk_window_set_default_size ptr width height))
            'set-application (lambda (application)
               (cond
                  ((eq? (type application) type-vptr)
                     (gtk_window_set_application ptr application))
                  ((function? application)
                     (gtk_window_set_application ptr (application 'ptr)))
                  (else
                     (runtime-error "GtkWindow: set-application: invalid application" application))))

            'add (lambda (widget)
               (when (function? widget)
                  (define child (widget 'widget))
                  (when child
                     (gtk_container_add ptr child))))

            'show-all (lambda ()
               (gtk_widget_show_all ptr))

         })

         ; apply options
         (when (ff? options)
            (if (options 'application #f)
               ((this 'set-application) (options 'application)))
            (if (options 'title #f)
               ((this 'set-title) (options 'title)))
            (if (options 'width (options 'height #f))
               ((this 'set-default-size) (options 'width) (options 'height)))
         )

         ; smart object
         (GtkThis this))

   ; defaults
   (define default-title "Window")

   ; main
   (case-lambda
      ((a1) (cond
               ; gtk_builder_get_object:
               ((eq? (type a1) type-vptr)
                  (make a1 #f))
               ; GtkApplication
               ((GtkThis? a1)
                  (make (gtk_window_new 0) {
                        'application a1
                     }))

               (else
                  (runtime-error "GtkWindow" "invalid argument")) ))

      ((a1 op) (cond
               ((and (GtkThis? a1) (ff? op))
                  (make (gtk_window_new
                           (case (op 'flags #f)
                              ('top-level 1)
                              ('popup 2)))
                        (put op
                           'application a1
                        )))
               (else
                  (runtime-error "GtkWindow" "invalid argument")) ))
   ))

   
))
