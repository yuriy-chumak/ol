(define-library (Gtk 3 Window)
   (export
      GtkWindow
   )
   (import
      (scheme base)

      (Gtk 3 Gtk)
      (Gtk 3 Application)
      ;; (Gtk 3 Widget)
      (Gtk 3 Container)
      
      (lib gtk-3 application)
      (lib gtk-3 window))

(begin
   (define GtkWindow
      (define (make ctor ptr options)
         (define base (GtkContainer ctor ptr options))
         (define this (ff-replace base {
            'class 'Window  'superclass 'Container
            'super base

            'Window ptr

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

            ; Get ...
            'get-application (lambda ()
               (GtkApplication (gtk_window_get_application ptr)))

            ; Sets or unsets the GtkApplication associated with the window.
            'set-application (lambda (application)
               (cond
                  ((eq? (type application) type-vptr)
                     (gtk_window_set_application ptr application))
                  ((GObject? application)
                     (gtk_window_set_application ptr (application 'Application)))
                  (else
                     (runtime-error "GtkWindow: set-application: invalid application" application))))
         }))

         ;; apply options
         (when (options 'application #f)
            ((this 'set-application) (options 'application)))
         (when (options 'title #f)
            ((this 'set-title) (options 'title)))
         (when (options 'width (options 'height #f))
            ((this 'set-default-size) (options 'width 640) (options 'height 480)))
         (when (options 'icon #f)
            ((this 'set-icon) (options 'icon)))

         ;; smart object
         (GObject this))

   ; defaults
   (define default-title "Window")

   ; main
   (case-lambda
      ((a1) (cond
               ((vptr? a1) ; (GtkWindow (gtk_builder_get_object ...))
                  (make make a1 #e))
               ; (GtkWindow Application)
               ((a1 'Application #f)
                  (make make (gtk_window_new 0) {
                        'application a1
                     }))
               (else
                  (runtime-error "GtkWindow: invalid argument" a1)) ))

      ((a1 op) (cond
               ((and (vptr? a1) (ff? op))
                  (make make a1 op))
               ((a1 'Application #f)
                  (cond
                     ; (Application "window-title")
                     ((string? op)
                        (make make (gtk_window_new 0) {
                           'title op
                           'application a1
                        }))
                     ; (Application options)
                     ((ff? op)
                        (make make (gtk_window_new (case (op 'flags #f)
                                                      ('top-level 1)
                                                      ('popup 2)))
                           (put op
                              'application a1
                           )))
                     (else
                        (error "GtkWindow" a1 op)) ))
               (else
                  (error "GtkWindow" a1 op)) ))
   ))
))
