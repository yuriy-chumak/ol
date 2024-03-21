(define-library (lib gtk-3 window)
   (export
      GtkWindow*

      gtk_window_new
      gtk_window_set_title
      gtk_window_set_default_size
      gtk_window_set_icon_name
      gtk_window_set_application
      gtk_window_get_application

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
   (define gtk_window_set_icon_name (GTK3 void "gtk_window_set_icon_name" GtkWindow* type-string))
   (define gtk_window_set_application (GTK3 void "gtk_window_set_application" GtkWindow* GtkApplication*))
   (define gtk_window_get_application (GTK3 GtkApplication* "gtk_window_get_application" GtkWindow*))

   (define gtk_window_get_size (GTK3 void "gtk_window_get_size" GtkWindow* (fft& gint) (fft& gint)))
   (define gtk_window_get_default_size (GTK3 void "gtk_window_get_default_size" GtkWindow* (fft& gint) (fft& gint)))
   (define gtk_window_resize (GTK3 void "gtk_window_resize" GtkWindow* gint gint))
   (define gtk_window_present (GTK3 void "gtk_window_present" GtkWindow*))

   ; lisp interface
   (define GtkWindow
      (define (make ptr options)
         (define base (GtkContainer ptr options))
         (define this (ff-replace base {

            ; Sets the title of the GtkWindow.
            'set-title (lambda (title)
               (gtk_window_set_title ptr title))

            ; Sets the default size of a window.
            'set-default-size (lambda (width height)
               (gtk_window_set_default_size ptr width height))

            ; Sets the icon for the window from a named themed icon.
            'set-icon (lambda (icon)
               (cond
                  ((string? icon)
                     (gtk_window_set_icon_name ptr icon))
                  (else
                     (runtime-error "GtkWindow.set-default-icon: invalid icon" icon))))

            ; Sets or unsets the GtkApplication associated with the window.
            'set-application (lambda (application)
               (cond
                  ((eq? (type application) type-vptr)
                     (gtk_window_set_application ptr application))
                  ((GObject? application)
                     (gtk_window_set_application ptr (application 'ptr)))
                  (else
                     (runtime-error "GtkWindow: set-application: invalid application" application))))

            ; internals
            'super base
         }))
         ; apply options
         (if (options 'application #f)
            ((this 'set-application) (options 'application)))
         (if (options 'title #f)
            ((this 'set-title) (options 'title)))
         (if (options 'width (options 'height #f))
            ((this 'set-default-size) (options 'width 640) (options 'height 480)))
         (if (options 'icon #f)
            ((this 'set-icon) (options 'icon)))

         ; smart object
         (GObject this))

   ; defaults
   (define default-title "Window")

   ; main
   (case-lambda
      ((a1) (cond
               ; gtk_builder_get_object:
               ((eq? (type a1) type-vptr)
                  (make a1 #e))
               ; GtkApplication
               ((GObject? a1)
                  (make (gtk_window_new 0) {
                        'application a1
                     }))
               (else
                  (runtime-error "GtkWindow: invalid argument" a1)) ))

      ((a1 op) (call/cc (lambda (return)
               (cond
                  ((and (eq? (type a1) type-vptr) (ff? op))
                     (return (make a1 op)))
                  ((GObject? a1) (cond
                     ((string? op)
                        (return
                           (make (gtk_window_new 0) {
                              'title op
                              'application a1
                           })))
                     ((ff? op)
                        (return
                           (make (gtk_window_new
                              (case (op 'flags #f)
                                 ('top-level 1)
                                 ('popup 2)))
                              (put op
                                 'application a1
                              )))) )))
                  (runtime-error "GtkWindow: invalid arguments" (cons a1 op))) ))
   ))
))
